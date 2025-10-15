module CryptLib (encryptRotation, decryptRotation, encryptVingenere, decryptVingenere) where
    import CryptLib.Internal (casedAlphabet, rtlen, removeDirt, readdDirt)
    import Data.List (elemIndex)

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