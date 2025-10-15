module CryptLib.Internal (casedAlphabet, rtlen, removeDirt, readdDirt) where
    import Data.Char (isLowerCase)
    import Data.List (findIndex)
    import Data.Maybe (isNothing)

    upperCaseAlphabet :: String
    upperCaseAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    lowerCaseAlphabet :: String
    lowerCaseAlphabet = "abcdefghijklmnopqrstuvwxyz"

    casedAlphabet :: Char -> String
    casedAlphabet c = if isLowerCase c then lowerCaseAlphabet else upperCaseAlphabet

    rtlen :: Int -> String -> String
    rtlen dlen str 
        | length str < dlen = rtlen dlen (str ++ str)
        | length str > dlen = take dlen str
        | otherwise = str

    isDirt :: Char -> Bool
    isDirt chr = isNothing $ findIndex (==chr) (casedAlphabet chr)

    removeDirt :: String -> String
    removeDirt str = filter (not . isDirt) str

    readdDirt :: String -> String -> String
    readdDirt str oreg = case findIndex (isDirt) oreg of
        Just pos -> readdDirt ((take pos str) ++ [oreg!!pos] ++ (drop pos str)) (take pos oreg ++ ['A'] ++ (drop (pos+1) oreg))
        Nothing -> str