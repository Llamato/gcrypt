module CryptLib.Internal (casedAlphabet, alphabetPos, invertAlphabet, rtlen, removeDirt, readdDirt, cdiv, rotate, substitute, apply) where
    import Data.Char (isLowerCase)
    import Data.List (findIndex, sortBy)
    import Data.Maybe (isNothing, fromMaybe)

    upperCaseAlphabet :: String
    upperCaseAlphabet = ['A' .. 'Z']

    lowerCaseAlphabet :: String
    lowerCaseAlphabet = "abcdefghijklmnopqrstuvwxyz"

    casedAlphabet :: Char -> String
    casedAlphabet c = if isLowerCase c then lowerCaseAlphabet else upperCaseAlphabet

    alphabetPos :: String -> Char -> Maybe Int
    alphabetPos alphabet char = findIndex (==char) alphabet

    invertAlphabet :: String -> String
    invertAlphabet alphabet =  map (\(i,_) -> upperCaseAlphabet!!i) (sortBy (\(_,av) (_,bv) -> compare av bv) (map (\(i, v) -> (i, fromMaybe 0 $ alphabetPos (casedAlphabet v) v)) (zip [0 :: Int ..] alphabet)))

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
    
    cdiv :: Integral a => a -> a -> a
    cdiv n d = (n + d -1) `div` d 

    rotate :: Int -> [a] -> [a]
    rotate _ [] = []
    rotate n xs = zipWith const (drop n (cycle xs)) xs

    substitute :: String -> Char -> String -> Maybe Char
    substitute inalph inchar outalph = case index of 
        Just i -> if i < length outalph 
                then Just (outalph !! i) 
                else Nothing
        Nothing -> Nothing
        where
            index = alphabetPos inalph inchar

    apply :: [(a -> b)] -> [a] -> [b]
    apply (f:fs) (x:xs) = (f x) : (apply fs xs)
    apply _ _ = []