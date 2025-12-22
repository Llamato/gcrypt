import CryptLib(encryptRotation, decryptRotation, encryptVingenere, decryptVingenere, encryptScytale, decryptScytale)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe, fromMaybe)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist)

data CryptError = NotEnoughArguments String Int Int

algOptions :: String -> String
algOptions algo
  | algo == "enrot" || algo == "rotation" = "options for rotation are <number of shifts> <plain text> use derot or negative number of shifts for decryption"
  | algo == "derot" = "options for rotation decryption are <number of shifts> <plain text> use enrot or negative number of shifts for encryption"
  | algo == "envin" || algo == "vingenere" = "options for vingenere encryption are <key> <plain text> use devin for decryption or vin for short"
  | algo == "devin" = "options for vingenere decryption are <key> <cipher text> use envin for encryption"
  | algo == "enscytale" || algo == "scytale" = "options for scytale encryption are <number of wraps> <plain text>"
  | algo == "descytale" || algo == "scytale" = "options for scytale decryption are <number of wraps> <cipher text>"
  | otherwise = "Algorithm " ++ algo ++ " not implemented yet"

runAlgorithm :: String -> [String] -> String 
runAlgorithm algo args
  | algo == "enrot" = CryptLib.encryptRotation (fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args))) (concat $ drop 1 args)
  | algo == "derot" = CryptLib.decryptRotation (fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args))) (concat $ drop 1 args)
  | algo == "envin" = CryptLib.encryptVingenere (fromMaybe "" (listToMaybe args)) (concat $ drop 1 args)
  | algo == "devin" = CryptLib.decryptVingenere (fromMaybe "" (listToMaybe args)) (concat $ drop 1 args)
  | algo == "enscytale" = CryptLib.encryptScytale (fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args))) (concat $ drop 1 args)
  | algo == "descytale" = CryptLib.decryptScytale (fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args))) (concat $ drop 1 args)
  | otherwise = "Algorithm not implemented yet"

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> putStrLn $ "Please use as follows -> gcrypt <algorithm name> [input stream] [algorithm parameters, ...]\ncurrently available algorithms are rotation (enrot, derot), Vingenere (envin, devin) and Scytale (enscytale, descytale)"
    1 -> putStrLn $ algOptions (head args)
    _ -> case args!!1 of
          "stdin" -> do
            contents <- getContents
            putStrLn $ runAlgorithm (head args) ((drop 2 args) ++ [contents])
          "file" -> do
              exists <- doesFileExist $ args!!2
              if exists then do
                contents <- readFile $ args!!2
                putStrLn $ runAlgorithm (head args) ((drop 3 args) ++ [contents])
              else
                hPutStrLn stderr ("Error: file " ++ args!!1 ++ " not found!")
          "params" -> putStrLn $ runAlgorithm (head args) (drop 2 args)
          _ -> do
            hPutStrLn stderr (args!!1 ++ " Is not a valid input stream. Valid input streams are [stdin, file, params]")