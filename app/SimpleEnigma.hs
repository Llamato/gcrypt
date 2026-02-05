module Main where
import CryptLib.Internal (invertAlphabet)
import CryptLib (prefabEnigmaRotors, EnigmaSetup(..), encryptEnigma, EnigmaRotor(..))
import Data.List (nub, elemIndex)
import Data.Char (toUpper, isLetter)
import Data.Maybe (fromMaybe)

--duplicates :: [a] -> [a]
--duplicates list = 

main :: IO ()
main = do
    putStrLn "=== Comprehensive Enigma Test ==="
    
    -- Setup: Rotors I, II, III at AAA, no plugboard
    let setup = EnigmaSetup {
            rotors = [prefabEnigmaRotors!!4, prefabEnigmaRotors!!3, prefabEnigmaRotors!!2],
            plugboard = []
        }
    
    -- TEST 1: No letter encrypts to itself
    putStrLn "\n1. Testing no self-encryption:"
    let alphabet = ['A'..'Z']
    let encrypted = encryptEnigma setup alphabet
    let selfPairs = filter (\(a,b) -> a == b) $ zip alphabet encrypted
    if null selfPairs
        then putStrLn "✓ PASS: No letter encrypts to itself"
        else putStrLn $ "✗ FAIL: These encrypt to themselves: " ++ show selfPairs
    
    -- TEST 2: Rotors step (AA should give different letters)
    putStrLn "\n2. Testing rotor stepping:"
    let aaResult = encryptEnigma setup "AA"
    if length aaResult == 2 && aaResult !! 0 /= aaResult !! 1
        then putStrLn $ "✓ PASS: AA -> " ++ aaResult ++ " (different letters)"
        else putStrLn $ "✗ FAIL: AA -> " ++ aaResult ++ " (same letters)"
    
    -- TEST 3: Encryption is reversible
    putStrLn "\n3. Testing reversibility:"
    let testPhrase = "HELLOWORLD"
    let encryptedPhrase = encryptEnigma setup testPhrase
    let decryptedPhrase = encryptEnigma setup encryptedPhrase
    
    putStrLn $ "Original:  " ++ testPhrase
    putStrLn $ "Encrypted: " ++ encryptedPhrase
    putStrLn $ "Decrypted: " ++ decryptedPhrase
    
    if testPhrase == decryptedPhrase
        then putStrLn "✓ PASS: Successfully decrypted"
        else putStrLn "✗ FAIL: Decryption doesn't match"
    
    -- TEST 4: Check rotor positions after use
    putStrLn "\n4. Testing rotor state:"
    putStrLn $ "Initial rotor positions: " ++ show (map position $ take 3 prefabEnigmaRotors)
    
    -- TEST 5: Known value test (if you have any)
    putStrLn "\n5. Quick manual check:"
    putStrLn $ "A -> ? (yours gives: " ++ encryptEnigma setup "A" ++ ")"
    putStrLn  $ "B -> ? (yours gives: " ++ encryptEnigma setup "B" ++ ")"
    putStrLn  $ "C -> ? (yours gives: " ++ encryptEnigma setup "C" ++ ")"
    
    -- TEST 6: All letters map to different letters
    putStrLn "\n6. Testing bijection (all outputs different):"
    putStrLn "All latters of the alphabet with the same setup"
    putStrLn $ map (\c -> head $ encryptEnigma setup [c]) ['A' .. 'Z']
    putStrLn "All latters of the alphabet with progressing setup (as a message)"
    putStrLn $ encryptEnigma setup alphabet
    let allOutputs = encryptEnigma setup alphabet
    let uniqueOutputs = nub allOutputs
    if length uniqueOutputs == 26
        then putStrLn "✓ PASS: All 26 outputs are unique"
        else putStrLn $ "✗ FAIL: Only " ++ show (length uniqueOutputs) ++ " unique outputs"

    --Test 7: Inverse wiring debugging
    putStrLn "\n=== Testing Inverse Wiring ==="
    
    let wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"  -- Rotor I
    let inv = invertAlphabet wiring
    
    putStrLn $ "Original:  " ++ wiring
    putStrLn $ "Inverse:   " ++ inv
    
    -- Test that forward then backward gives original
    let alphabet = ['A'..'Z']
    let results = map (\c -> 
            let idx = fromMaybe 0 $ elemIndex c alphabet
                fwd = wiring !! idx
                fwdIdx = fromMaybe 0 $ elemIndex fwd alphabet
                bwd = inv !! fwdIdx
            in (c, fwd, bwd)) alphabet
    
    -- Check if any letter doesn't round-trip
    let errors = filter (\(c, fwd, bwd) -> c /= bwd) results
    
    if null errors
        then putStrLn "✓ Inverse wiring correct"
        else do
            putStrLn "✗ Inverse wiring errors:"
            mapM_ (\(c, fwd, bwd) -> 
                putStrLn $ c : " -> " ++ [fwd] ++ " -> " ++ [bwd] ++ " (should be " ++ [c] ++ ")") 
                (take 5 errors)
    
    putStrLn "\n=== Test Complete ==="