{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.State
import Data.Aeson
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as LBS

data IRKitData = IRKitData {
    format :: String,
    freq   :: Int,
    data_  :: [Int] -- dataは予約語なので
} deriving (Show)

instance FromJSON IRKitData where
    parseJSON (Object x) = IRKitData <$> x .: "format" <*> x.: "freq" <*> x.: "data"
    parseJSON _          = fail "Expected an Object"

instance ToJSON IRKitData where
    toJSON irkitData = object
        [
          "format" .= format irkitData,
          "freq"   .= freq   irkitData,
          "data"   .= data_  irkitData
        ]


-- マイクロ秒単位の時間を単位時間の何倍かで丸めます
-- 単位時間は元データのうち350から500マイクロ秒の範囲のものを最大30個サンプリングし
-- その平均値とします
quantize :: [Int] -> [Int]
quantize l = map (q t0) l where
    q t x = floor((fromIntegral x) / t + 0.5) -- x / t を四捨五入
    average xs = realToFrac (sum xs) / genericLength xs
    t0 = average (take 30 (filter p l))
    p x =(x >= 350) && (x <= 500)

data OnOff = OnOff {
    on  :: Int,
    off :: Int
} deriving (Show)

-- オンとオフの時間をひとまとめにします
onoff :: [Int] -> [OnOff]
onoff []       = []
onoff (x:[])   = [OnOff x 0]
onoff (x:y:xs) = (OnOff x y) : onoff xs

data AHEAToken = Bit Int | Leader | RepeatToken | Trailer Int | Invalid Int Int
    deriving (Show)

-- オンオフ時間の組をトークン化します
tokenize :: OnOff -> AHEAToken
tokenize (OnOff 1  1)   = Bit 0
tokenize (OnOff 1  3)   = Bit 1
tokenize (OnOff 8  4)   = Leader
tokenize (OnOff 8  8)   = RepeatToken
tokenize (OnOff 1  off) = Trailer off
tokenize (OnOff on off) = Invalid on off

data AHEAData =
    AHEAFrame {
        customerCode :: Int,
        parity       :: Int,
        dat          :: [Int]
    } |
    AHEARepeat |
    AHEAUnexpected AHEAToken
    deriving (Show)


shift :: State [a] (Maybe a)
shift = do
    r <- get
    case r of
        x:xs -> do
            put xs
            return (Just x)
        _ -> do
            return Nothing

-- アクションが失敗(Nothing)を返したときはアクション内で状態変化があっても呼ぶ前の状態に戻す
try :: (State s (Maybe a)) -> (State s (Maybe a))
try action = do
    s <- get
    result <- action
    case result of
        Nothing -> do
            put s
            return Nothing
        _ -> do
            return result

takeBitList :: Int -> State [AHEAToken] (Maybe [Int])
takeBitList 0 = do
    return (Just [])
takeBitList n = try $ do
    b  <- shift
    bs <- takeBitList (n - 1)
    case (b, bs) of
        (Just (Bit b'), Just bs') -> return (Just (b':bs'))
        _                         -> return Nothing

takeBit :: Int -> State [AHEAToken] (Maybe Int)
takeBit n = try $ do
    do
        bits <- takeBitList n
        case bits of
            Just bits' -> return $ Just $ bitsToInt bits'
            _          -> return $ Nothing

bitsToInt :: [Int] -> Int
bitsToInt [] = 0
bitsToInt (b:bs) = b + 2 * bitsToInt bs

takeLeader:: State [AHEAToken] (Maybe ())
takeLeader = try $do
    do
        x <- shift
        case x of
            Just(Leader) -> return $ Just ()
            _            -> return $ Nothing

takeTrailer:: State [AHEAToken] (Maybe ())
takeTrailer = try $ do
    x <- shift
    case x of
        Just(Trailer _) -> return $ Just ()
        _               -> return $ Nothing

takeOctets :: State [AHEAToken] [Int]
takeOctets = do
    octet <- takeBit 8
    case octet of
        Just octet' -> do
            octets <- takeOctets
            return (octet':octets)
        _ -> 
            return []

takeAHEAFrame :: State [AHEAToken] (Maybe AHEAData)
takeAHEAFrame = try $ do
    leader       <- takeLeader
    customerCode <- takeBit 16
    parity       <- takeBit 4
    d0           <- takeBit 4
    ds           <- takeOctets
    trailer      <- takeTrailer
    case (leader, customerCode, parity, d0, trailer) of
        (Just _, Just customerCode', Just parity', Just d0', Just _) ->
            return $ Just $ AHEAFrame customerCode'  parity' (d0':ds)
        _ ->
            return $ Nothing

takeAHEARepeat :: State [AHEAToken] (Maybe AHEAData)
takeAHEARepeat = try $ do
    repeat <- shift
    case repeat of
        Just RepeatToken -> return $ Just AHEARepeat
        _                -> return $ Nothing

takeAHEAUnexpected :: State [AHEAToken] (Maybe AHEAData)
takeAHEAUnexpected = try $ do
    token <- shift
    case token of
        Just(token') -> return $ Just $ AHEAUnexpected token'
        _            -> return $ Nothing

-- 引数に与えられたアクションを順に適用して最初に成功した物を返す。
-- 全て失敗したら失敗
tryAny :: [State s (Maybe a)] -> State s (Maybe a)
tryAny [] = do return Nothing 
tryAny (act:acts) = try $ do
    r <- act
    case r of
        Just _ -> 
            return r
        _ -> do
            r' <- tryAny acts
            return r'

-- Maybeを返すactionが成功する間状態にactionを適用していき得られた結果をリストにして返す
tryWhile :: (State s (Maybe a)) -> State s [a]
tryWhile action = do
    r  <- action
    case r of
        Just r'-> do
            rs <- tryWhile action
            return (r':rs)
        _ -> do
            return []

-- AHEAフォーマットの赤外線リモコンによるIRKit形式のjson文字列をデコードします
decodeAHEAJson :: LBS.ByteString -> [AHEAData]
decodeAHEAJson json = evalState action tokenList where
    action    = tryWhile $ tryAny $ [takeAHEAFrame, takeAHEARepeat, takeAHEAUnexpected]
    tokenList = map tokenize $ onoff $ quantize $ data_ $ fromJust $ decode $ json

-- AHEAフォーマットの赤外線リモコンによるIRKit形式のjson文字列が格納されたファイルを読んでデコード結果を出力します
decodeAHEAJsonFile :: String -> IO()
decodeAHEAJsonFile filename = do 
    jsonString <- LBS.readFile filename
    putStrLn $ show $ decodeAHEAJson $ jsonString

