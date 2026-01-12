module CryptLib (encryptRotation, decryptRotation, encryptVingenere, decryptVingenere, encryptScytale, decryptScytale, encryptOneTimePad, decryptOneTimePad) where
    import CryptLib.Internal (casedAlphabet, rtlen, removeDirt, readdDirt, cdiv)
    import Data.List (elemIndex, transpose)
    import Data.List.Grouping (splitEvery)
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

    encryptOneTimePad :: String -> String -> [Int]
    encryptOneTimePad pad txt =  zipWith (\pc tc -> ((ord pc) `xor` (ord tc))) (cycle pad) txt

    decryptOneTimePad :: String -> [Int] -> String
    decryptOneTimePad pad nums = zipWith (\pc num -> chr ((ord pc) `xor` num)) (cycle pad) nums