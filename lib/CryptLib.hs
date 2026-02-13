module CryptLib (encryptRotation, decryptRotation, encryptVingenere, decryptVingenere, encryptScytale, decryptScytale, encryptXorOnetimePad, decryptXorOneTimePad, encryptModOneTimePad, decryptModOneTimePad, encryptFeistel, decryptFeistel) where
    import CryptLib.Internal (casedAlphabet, rtlen, removeDirt, readdDirt, cdiv, pairToList, swapPair, halves, padBlock, feistelRoundKeys, feistelNetwork)
    import Data.List (elemIndex, transpose)
    import Data.List.Grouping (splitEvery)
    import Data.List.Split (chunksOf)
    import Data.Char (ord, chr)
    import Data.Bits (xor)
    
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

    encryptXorOnetimePad :: String -> String -> [Int]
    encryptXorOnetimePad pad txt = zipWith (\pc tc -> ((ord pc) `xor` (ord tc))) (cycle pad) txt

    decryptXorOnetimePad :: String -> [Int] -> String
    decryptXorOnetimePad pad nums = zipWith (\pc num -> chr ((ord pc) `xor` num)) (cycle pad) nums

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

    encryptFeistel :: String -> String -> [Int]
    encryptFeistel key text = map ord $ concatMap (\textHalfBlock -> concat . pairToList $ feistelNetwork textHalfBlock roundKeys) textHalfBlocks
        where
            textBlocks = chunksOf blockSizeChars text
            roundKeys = feistelRoundKeys key blockSizeChars rounds
            textHalfBlocks = map halves textBlocks
            blockSizeChars = 4 
            rounds = 16

    decryptFeistel :: String -> [Int] -> String
    decryptFeistel key nums = concatMap (\swappedTextHalfBlock -> concat . pairToList . swapPair $ feistelNetwork swappedTextHalfBlock reversedRoundKeys) swappedTextHalfBlocks
        where
            numBlocks = chunksOf blockSizeChars nums
            roundKeys = feistelRoundKeys key blockSizeChars rounds
            reversedRoundKeys = reverse $ roundKeys
            numHalfBlocks = map halves numBlocks
            textHalfBlocks = map (\block -> (map chr $ fst block, map chr $ snd block)) numHalfBlocks
            swappedTextHalfBlocks = map swapPair textHalfBlocks
            blockSizeChars = 4 
            rounds = 16