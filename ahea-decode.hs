{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Functor
import Data.List
import Data.Maybe
import Control.Applicative
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
    q t x = floor((fromIntegral x) / t + 0.5)
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

decodeTokenList :: [AHEAToken] -> [AHEAData]
decodeTokenList [] = []
decodeTokenList (Leader:xs) = frame:(decodeTokenList ws) where
    (customerCode, ys) = takeBit 16 xs
    (parity, zs) = takeBit 4 ys
    (data0, us) = takeBit 4 zs
    (dat, vs) = takeBytes us
    ws = takeTrailer vs
    frame = AHEAFrame customerCode parity (data0:dat)
decodeTokenList (RepeatToken:xs) = AHEARepeat:(decodeTokenList xs)
decodeTokenList (x:xs) = (AHEAUnexpected x):(decodeTokenList xs)

-- AHEATokenの先頭から指定されたビット数の整数を取り出します
takeBit :: Int -> [AHEAToken] -> (Int, [AHEAToken])
takeBit n xs = fromJust(takeBitMaybe n xs)

takeBitMaybe :: Int -> [AHEAToken] -> Maybe (Int, [AHEAToken])
takeBitMaybe n xs = fmap (\(bits, xs') -> (bitsToInt bits, xs')) (tb n xs) where
    tb 0 xs           = Just ([], xs)
    tb n ((Bit b):xs) = fmap (\(bs,xs)->(b:bs,xs)) (tb (n-1) xs)
    tb _ _            = Nothing

-- ビットのリストを整数に変換します。
-- リストの先頭が2^0で以下2^1, 2^2, ...と続くものとします。
bitsToInt :: [Int] -> Int
bitsToInt xs = foldr (\x->(\y->x + 2 * y)) 0 xs

-- AHEATokenリストの先頭から8ビットずつあるだけ整数を取り出します
takeBytes :: [AHEAToken] -> ([Int], [AHEAToken])
takeBytes xs = case takeBitMaybe 8 xs of
    Nothing       -> ([], xs)
    Just (n, xs') -> (n:ns, xs'') where (ns, xs'') = takeBytes xs'

-- Trailerを取り出します。なければエラー
takeTrailer ((Trailer _):xs) = xs
takeTrailer _                = fail "no trailer"

-- AHEAフォーマットの赤外線リモコンによるIRKit形式のjson文字列をデコードします
decodeAHEAJson :: LBS.ByteString -> [AHEAData]
decodeAHEAJson json = decodeTokenList (map tokenize (onoff (quantize (data_ (fromJust (decode json))))))

-- AHEAフォーマットの赤外線リモコンによるIRKit形式のjson文字列が格納されたファイルを読んでデコード結果を出力します
decodeAHEAJsonFile :: String -> IO()
decodeAHEAJsonFile filename = do 
    jsonString <- LBS.readFile filename
    putStr $ show $ decodeAHEAJson $ jsonString

