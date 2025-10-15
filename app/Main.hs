import CryptLib(encryptRotation, decryptRotation, encryptVingenere, decryptVingenere)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe, fromMaybe)

algOptions :: String -> String
algOptions algo
  | algo == "enrot" || algo == "rotation" = "options for rotation are <number of shifts> <plain text> use derot or negative number of shifts for decryption"
  | algo == "derot" = "options for rotation decryption are <number of shifts> <plain text> use enrot or negative number of shifts for encryption"
  | algo == "envin" || algo == "vingenere" = "options for vingenere encryption are <key> <plain text> use devin for decryption or vin for short"
  | algo == "devin" = "options for vingenere decryption are <key> <cipher text> use envin for encryption"
  | otherwise = "Algorithm not implemented yet"

runAlgorithm :: String -> [String] -> String 
runAlgorithm algo args
  | algo == "enrot" = CryptLib.encryptRotation (fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args))) (concat $ drop 1 args)
  | algo == "derot" = CryptLib.decryptRotation ((fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args)))) (concat $ drop 1 args)
  | algo == "envin" = CryptLib.encryptVingenere (fromMaybe "" (listToMaybe args)) (concat $ drop 1 args)
  | algo == "devin" = CryptLib.decryptVingenere (fromMaybe "" (listToMaybe args)) (concat $ drop 1 args)
  | otherwise = "Algorithm not implemented yet"

main :: IO ()
main = do
  args <- getArgs
  putStrLn (case length args of
    0 -> "Welcome to gcrypt! Please use as follows.\ngcrypt <algorithm> [algorithm option 1, option 2, ...].\n"
    1 -> algOptions (head args)
    _ -> runAlgorithm (head args) (drop 1 args))
  