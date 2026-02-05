import CryptLib(encryptRotation, decryptRotation, encryptVingenere, decryptVingenere, encryptScytale, decryptScytale, encryptOneTimePad, decryptOneTimePad, prefabEnigmaRotors, EnigmaSetup(..), EnigmaRotor(..))
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe, fromMaybe, catMaybes, mapMaybe)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Char (isAlpha)
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
  | algo == "enonetimepad" = "options for one time pad are <pad String> <plain text String>"
  | algo == "deonetimepad" = "options for one time pad are <pad String> <[cipher numbers]>"
  | algo == "enenigma" || algo == "deenigma" = "options for engima encryption are (rotorNr,rotorPosition),(rotorNr,rotorPosition),(plugchar,plugchar)... <plaintext>"
  | otherwise = "Algorithm " ++ algo ++ " not implemented yet"

runAlgorithm :: String -> [String] -> String 
runAlgorithm algo args
  | algo == "enrot" = CryptLib.encryptRotation (fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args))) (concat $ drop 1 args)
  | algo == "derot" = CryptLib.decryptRotation (fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args))) (concat $ drop 1 args)
  | algo == "envin" = CryptLib.encryptVingenere (fromMaybe "" (listToMaybe args)) (concat $ drop 1 args)
  | algo == "devin" = CryptLib.decryptVingenere (fromMaybe "" (listToMaybe args)) (concat $ drop 1 args)
  | algo == "enscytale" = CryptLib.encryptScytale (fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args))) (concat $ drop 1 args)
  | algo == "descytale" = CryptLib.decryptScytale (fromMaybe 0 (readMaybe $ fromMaybe "0" (listToMaybe args))) (concat $ drop 1 args)
  | algo == "enonetimepad" =  intercalate " " $ map (show) (CryptLib.encryptOneTimePad (fromMaybe "" (listToMaybe args)) (concat $ drop 1 args))
  | algo == "deonetimepad" =  CryptLib.decryptOneTimePad (fromMaybe "" (listToMaybe args)) (map (\arg -> fromMaybe 0 $ readMaybe arg) (drop 1 args))
--  | algo == "enenigma" = CryptLib.encryptEnigma (parseEnigmaArgs args) (fromMaybe "" $ listToMaybe $ drop 2 args)
--  | algo == "enenigma" || algo == "deenigma" = testSimple
  | otherwise = "Algorithm not implemented yet"

--show (show (parseEnigmaArgs args) ++ "\n" ++ show (drop 2 args))
--CryptLib.encryptEnigma (parseEnigmaArgs args)

parseEnigmaArgs :: [String] -> EnigmaSetup
parseEnigmaArgs args = EnigmaSetup {
  rotors = takeElementsAtIndices prefabEnigmaRotors $ map (\i -> i-1) (parseIntList rotorListArg),
  plugboard = parseTupleList plugboardListArg
}
  where
    rotorListArg = fromMaybe "" $ listToMaybe args
    plugboardListArg = fromMaybe [] (listToMaybe $ drop 1 args)

takeElementsAtIndices :: [a] -> [Int] -> [a]
takeElementsAtIndices xs indices = [xs !! i | i <- indices]

parseIntList :: String -> [Int]
parseIntList arg = catMaybes $ map readMaybe $ splitOn "," arg

parseTupleList :: String -> [(Char, Char)]
parseTupleList = mapMaybe parseTuple . splitOn ","
  where
    parseTuple :: String -> Maybe (Char, Char)
    parseTuple str = case clean str of
        [a, b] | isAlpha a && isAlpha b -> Just (a, b)
        _ -> Nothing
    
    clean :: String -> String
    clean = filter (`notElem` "() ")

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> putStrLn $ "Please use as follows -> gcrypt <algorithm name> [input stream] [algorithm parameters, ...]\ncurrently available algorithms are rotation (enrot, derot), Vingenere (envin, devin), scytale (enscytale, descytale), xor one time padding (onetimepad) and enigma (enenigma, deenigma)"
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