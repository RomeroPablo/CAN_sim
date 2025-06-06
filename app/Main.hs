import qualified Data.ByteString.Char8 as B
import qualified Control.Concurrent     (threadDelay)
import           System.Hardware.Serialport
import           System.Random          (randomRIO)
import           Text.Printf            (printf)
import           Control.Monad          (forever, forM_, replicateM)
import           Data.Word              (Word16)

serialSetting :: SerialPortSettings
serialSetting = defaultSerialSettings
    { commSpeed   = CS115200
    , bitsPerWord = 8
    , stopb       = One
    , parity      = NoParity
    , flowControl = NoFlowControl
    }

genRandomHex :: Int -> IO String
genRandomHex nBytes
    | nBytes <= 0 = return ""
    | nBytes >  8 = error "genRandomHex: nBytes must be between 0 and 8"
    | otherwise   = replicateM (2 * nBytes) randomHexChar
    where
      hexDigits = "0123456789ABCDEF"
      randomHexChar :: IO Char
      randomHexChar = do
        idx <- randomRIO (0, length hexDigits - 1)
        return (hexDigits !! idx)

-- | Build a SLCAN frame:
-- “t<ID><DL><DATA>\r”
makeMsg :: Word16 -> Word16 -> String -> String
makeMsg canId dataLen hexData =
  -- printf "%03X" :: Word16 -> String, e.g. 0x123 -> "123"
  "t" ++ printf "%03X" canId ++ printf "%01X" dataLen ++ hexData ++ "\r"

main :: IO ()
main = do
  -- Replace "/dev/ttyUSB0" with your actual port
  putStrLn "[+] Starting Connection "
  withSerial "/dev/pts/1" serialSetting $ \serial -> do

    -- Forever: for each i in [1..0x7FF], pick a random payload, send SLCAN, wait 0.1 ms, then next
    forever $ do
      forM_ [1 .. 0x7FF :: Word16] $ \canId -> do
        nBytes <- randomRIO (0, 8)
        hexData <- genRandomHex nBytes

        let dataLen  = fromIntegral nBytes       :: Word16
            strMsg   = makeMsg canId dataLen hexData
            bsMsg    = B.pack strMsg

        _ <- send serial bsMsg
        putStrLn $ "[+] Sent ID=0x" ++ printf "%03X" canId
                 ++ "  DL=" ++ show nBytes
                 ++ "  data=" ++ hexData

        Control.Concurrent.threadDelay 10000
