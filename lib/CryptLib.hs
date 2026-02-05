module CryptLib (encryptRotation, decryptRotation, encryptVingenere, decryptVingenere, encryptScytale, decryptScytale, encryptOneTimePad, decryptOneTimePad, prefabEnigmaRotors, encryptEnigma, EnigmaSetup(..), EnigmaRotor(..)) where
    import CryptLib.Internal (casedAlphabet, alphabetPos, invertAlphabet, rtlen, removeDirt, readdDirt, cdiv, rotate, substitute, apply)
    import Data.List (elemIndex, transpose, findIndex, find, mapAccumL, foldl', sortOn)
    import Data.List.Grouping (splitEvery)
    import Data.Char (ord, chr, toUpper)
    import Data.Bits (xor)
    import Data.Maybe (fromMaybe)
    import Control.Monad.State ( gets, modify, runState, MonadState(get), State )

    encryptRotation :: Int -> String -> String
    encryptRotation r txt = map (
        \char -> case elemIndex char (casedAlphabet char) of 
            Just index -> (casedAlphabet char)!!((index+r) `mod` (length $ casedAlphabet char))
            Nothing -> char
        ) txt

    decryptRotation :: Int -> String -> String
    decryptRotation r txt = encryptRotation (-r) txt

    encryptVingenere :: String -> String -> String
    encryptVingenere key txt = readdDirt (map (\(k, v) ->
            case (elemIndex k (casedAlphabet k), elemIndex v (casedAlphabet v)) of 
                (Just ki, Just vi) -> 
                    ((casedAlphabet v)!!((vi+ki) `mod` (length $ casedAlphabet v)))
                _ -> v) (zip (rtlen (length $ removeDirt txt) (removeDirt key)) (removeDirt txt))) txt
    
    decryptVingenere :: String -> String -> String
    decryptVingenere key txt = readdDirt (map (\(k, v) ->
            case (elemIndex k (casedAlphabet k), elemIndex v (casedAlphabet v)) of 
                (Just ki, Just vi) -> 
                    ((casedAlphabet v)!!((vi-ki) `mod` (length $ casedAlphabet v)))
                _ -> v) (zip (rtlen (length $ removeDirt txt) (removeDirt key)) (removeDirt txt))) txt
    
    encryptScytale :: Int -> String -> String
    encryptScytale wraps txt = concat $ transpose $ splitEvery wraps (removeDirt txt)
    
    decryptScytale :: Int -> String -> String
    decryptScytale wraps text = encryptScytale ((length text) `cdiv` wraps) text

    encryptOneTimePad :: String -> String -> [Int]
    encryptOneTimePad pad txt =  zipWith (\pc tc -> ((ord pc) `xor` (ord tc))) (cycle pad) txt

    decryptOneTimePad :: String -> [Int] -> String
    decryptOneTimePad pad nums = zipWith (\pc num -> chr ((ord pc) `xor` num)) (cycle pad) nums

    data EnigmaRotor = EnigmaRotor {
        position :: Int,
        nodgePosition :: Int,
        wiring :: String
    } deriving (Show)

    enigmaUKW2 :: String
    enigmaUKW2 = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

    data EnigmaSetup = EnigmaSetup {
        rotors :: [EnigmaRotor],
        plugboard :: [(Char, Char)]
    } deriving (Show)

    stepRightmost :: [EnigmaRotor] -> [EnigmaRotor]
    stepRightmost [] = []
    stepRightmost (r:rs) = 
        r { position = (position r + 1) `mod` length (wiring r) } : rs

    getPositionsM :: State [EnigmaRotor] [Int]
    getPositionsM = gets (map position)

    enigmaRotorPositionByLabel :: Char -> Int
    enigmaRotorPositionByLabel label = fromMaybe 0 $ findIndex (==label) $ casedAlphabet label

    prefabEnigmaRotors :: [EnigmaRotor]
    prefabEnigmaRotors = reverse [
            EnigmaRotor {
                position = 0,
                nodgePosition = enigmaRotorPositionByLabel 'Q',
                wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
            },
            EnigmaRotor {
                position = 0,
                nodgePosition = enigmaRotorPositionByLabel 'E',
                wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
            },
            EnigmaRotor {
                position = 0,
                nodgePosition = enigmaRotorPositionByLabel 'V',
                wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
            },
            EnigmaRotor {
                position = 0,
                nodgePosition = enigmaRotorPositionByLabel 'J',
                wiring = "ESOVPZJAYQUIRHXLNFTGKDCMWB"
            },
            EnigmaRotor {
                position = 0,
                nodgePosition = enigmaRotorPositionByLabel 'Z',
                wiring = "VZBRGITYUPSDNHLXAWJMQOFECK"
            }
        ]

    stepRightmostM :: State [EnigmaRotor] ()
    stepRightmostM = modify stepRightmost

    -- Reflector function
    applyReflector :: Char -> Char
    applyReflector c = 
        let alphabet = ['A'..'Z']
            idx = fromMaybe 0 $ elemIndex (toUpper c) alphabet
        in enigmaUKW2 !! idx

    invertedAlphabet :: String -> String
    invertedAlphabet wiring =
        let alphabet = ['A'..'Z']
            pairs = zip alphabet wiring
            -- Create mapping from output to input
            inverseMap = [(out, in') | (in', out) <- pairs]
            -- Sort by output letter
            sorted = sortOn fst inverseMap
        in map snd sorted

    processCharM :: Char -> State [EnigmaRotor] Char
    processCharM c = do 
        rotors <- get
        let 
            forwardTransformed = applyEnigmaRotors c rotors
            reflected = applyReflector forwardTransformed 
            inverseRotors = map (\rotor -> EnigmaRotor {
                position = position rotor,
                nodgePosition = nodgePosition rotor,
                wiring = invertedAlphabet $ wiring rotor
            }) rotors
            backwardsTransformed = applyEnigmaRotors reflected (reverse inverseRotors)
        stepRightmostM
        return backwardsTransformed

    applyEnigmaWiring :: EnigmaRotor -> Char -> Char
    applyEnigmaWiring rotor inputChar = 
        let alphabet = ['A'..'Z']
            inputIdx = fromMaybe 0 $ elemIndex (toUpper inputChar) alphabet
            offsetIdx = (inputIdx + position rotor) `mod` (length $ casedAlphabet inputChar)
            wiredChar = wiring rotor !! offsetIdx
            wiredIdx = fromMaybe 0 $ elemIndex wiredChar alphabet
            outputIdx = (wiredIdx - position rotor + (length $ casedAlphabet inputChar)) `mod` (length $ casedAlphabet inputChar)
        in alphabet !! outputIdx
        
    applyEnigmaRotors :: Char -> [EnigmaRotor] -> Char
    applyEnigmaRotors ch rs = foldl' (\c rotor -> applyEnigmaWiring rotor c) ch rs

    applyPlugboard :: String -> [(Char, Char)] -> String
    applyPlugboard str plugs = map plugChar str
      where
        plugChar c = 
            case find (\(a,b) -> a == c || b == c) plugs of
                Just (a,b) -> if a == c then b else a
                Nothing -> c

    encryptEnigma :: EnigmaSetup -> String -> String
    encryptEnigma setup plaintext = 
        let plugboardPairs = plugboard setup
            inputPlugged = applyPlugboard plaintext plugboardPairs
            (_, processedText) = mapAccumL processChar (rotors setup) inputPlugged
            outputPlugged = applyPlugboard processedText plugboardPairs
        in outputPlugged
      where
        processChar :: [EnigmaRotor] -> Char -> ([EnigmaRotor], Char)
        processChar currentRotors char = if toUpper char `elem` ['A' .. 'Z'] then
            let (translatedChar, nextRotors) = runState (processCharM char) currentRotors 
            in (nextRotors, translatedChar)
            else (currentRotors, char)