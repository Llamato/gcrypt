module CryptLib (encryptRotation, decryptRotation, encryptVingenere, decryptVingenere, encryptScytale, decryptScytale, encryptXorOneTimePad, decryptXorOneTimePad, encryptModOneTimePad, decryptModOneTimePad, encryptFeistel, decryptFeistel, padPKCS7, unpadPKCS7) where
    import CryptLib.Internal (casedAlphabet, rtlen, removeDirt, readdDirt, cdiv, pairToList, swapPair, halves, padBlock, unpadBlock, trimBits, feistelRoundKeys, feistelNetwork)
    import Data.List (elemIndex, transpose)
    import Data.List.Grouping (splitEvery)
    import Data.List.Split (chunksOf)
    import Data.Char (ord, chr)
    import Data.Bits (xor, shiftL)
    
    encryptRotation :: Int -> String -> String
    encryptRotation r txt = map (
        \chr -> case elemIndex chr (casedAlphabet chr) of 
        Just index -> (casedAlphabet chr)!!((index+r) `mod` (length $ casedAlphabet chr))
        Nothing -> chr
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

    encryptXorOneTimePad :: String -> String -> [Int]
    encryptXorOneTimePad pad txt = zipWith (\pc tc -> ((ord pc) `xor` (ord tc))) (cycle pad) txt

    decryptXorOneTimePad :: String -> [Int] -> String
    decryptXorOneTimePad pad nums = zipWith (\pc num -> chr ((ord pc) `xor` num)) (cycle pad) nums

    encryptModOneTimePad :: String -> String -> String
    encryptModOneTimePad pad txt = zipWith (\pc tc -> 
        case (elemIndex pc $ casedAlphabet pc, elemIndex tc $ casedAlphabet tc) of 
            (Just pi, Just ti) -> (casedAlphabet tc)!!((ti+pi) `mod` (length $ casedAlphabet tc))
            _ -> tc) (cycle pad) txt

    decryptModOneTimePad :: String -> String -> String
    decryptModOneTimePad pad txt = zipWith (\pc tc ->
        case (elemIndex pc $ casedAlphabet pc, elemIndex tc $ casedAlphabet tc) of
            (Just pi, Just ti) -> (casedAlphabet tc)!!((ti-pi) `mod` (length $ casedAlphabet tc))
            _ -> tc) (cycle pad) txt

    padPKCS7 :: Int -> String -> String
    padPKCS7 blockSize blockData = blockData ++ replicate additionalBytes (chr additionalBytes)
        where 
            remainingBlockBytes = length blockData `mod` blockSize
            additionalBytes = blockSize - remainingBlockBytes

    unpadPKCS7 :: String -> String
    unpadPKCS7 blockData = take (length blockData - padSize) blockData
        where
            padSize = ord $ last blockData

    encryptFeistel :: String -> String -> [Int]
    encryptFeistel key text = map ord $ concatMap (\textHalfBlock -> concat . pairToList $ feistelNetwork textHalfBlock roundKeys) textHalfBlocks
        where
            padedInput = padPKCS7 blockSize text
            textBlocks = chunksOf blockSize padedInput
            roundKeys = feistelRoundKeys key blockSize rounds
            textHalfBlocks = map halves textBlocks
            blockSize = 4 
            rounds = 16

    decryptFeistel :: String -> [Int] -> String
    decryptFeistel key nums = unpadPKCS7 paddedOutput
        where
            text = map chr nums
            textBlocks = chunksOf blockSize text
            roundKeys = feistelRoundKeys key blockSize rounds
            reversedRoundKeys = reverse $ roundKeys
            textHalfBlocks = map halves textBlocks
            swappedTextHalfBlocks = map swapPair textHalfBlocks
            paddedOutput = concatMap (\swappedTextHalfBlock -> concat . pairToList . swapPair $ feistelNetwork swappedTextHalfBlock reversedRoundKeys) swappedTextHalfBlocks
            blockSize = 4 
            rounds = 16

    --encryptSDES :: String -> String -> [Int]
    --encryptSDES key text = 
    --    where
    --        trimmedKey = trimBits 10 key
    --        keyHalves = halves trimmedKey
    --        shiftedhalves = (shiftL 1 $ fst keyHalves, shiftL 1 $ snd keyHalves)
    --        recombinedhalves = [fst keyHalves, snd keyHalves]
            
    
    --encryptDES :: String -> String -> [Int]
    --encryptDES key text = 
    
    --decryptDES :: String -> [Int] -> String
    --decryptDES key nums = 