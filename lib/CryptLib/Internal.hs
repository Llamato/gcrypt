module CryptLib.Internal (casedAlphabet, rtlen, removeDirt, readdDirt, cdiv, pairToList, swapPair, halves, padBlock, feistelRoundKeys, feistelNetwork) where
    import Data.Char (isLowerCase)
    import Data.List (findIndex)
    import Data.List.Split (chunksOf)
    import Data.Maybe (isNothing)
    import Data.Char (ord, chr)
    import Data.Bits (xor)
    
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
    isDirt char = isNothing $ findIndex (==char) (casedAlphabet char)

    removeDirt :: String -> String
    removeDirt str = filter (not . isDirt) str

    readdDirt :: String -> String -> String
    readdDirt str oreg = case findIndex (isDirt) oreg of
        Just pos -> readdDirt ((take pos str) ++ [oreg!!pos] ++ (drop pos str)) (take pos oreg ++ ['A'] ++ (drop (pos+1) oreg))
        Nothing -> str
    
    cdiv :: Integral a => a -> a -> a
    cdiv n d = (n + d -1) `div` d 

    pairToList :: (a, a) -> [a]
    pairToList (l, r) = [l, r]

    swapPair :: (a, a) -> (a, a)
    swapPair (l, r) = (r, l)

    halves :: [a] -> ([a], [a])
    halves list = (take halflength list, drop halflength list)
        where 
            halflength = length list `div` 2
    
    padBlock :: Int -> String -> String
    padBlock blocksize str
        | length str `mod` blocksize == 0 = str
        | otherwise = str ++ replicate ((blocksize - length str) `mod` blocksize) ' '

    strxor :: String -> String -> String
    strxor str1 str2 = map chr $ zipWith xor (map ord str1) (map ord str2)

    feistelRoundKeys :: String -> Int -> Int -> [String]
    feistelRoundKeys key blocksizechars rounds = map (padBlock halfblocksize) (take rounds $ cycle $ chunksOf halfblocksize key)
        where 
            halfblocksize = blocksizechars `div` 2

    feistelRound :: (String, String) -> String -> (String, String)
    feistelRound (l1, r1) k1 = (l2, r2)
        where 
            f1 = r1 `strxor` k1
            r2 = l1 `strxor` f1
            l2 = r1

    feistelNetwork :: (String, String) -> [String] -> (String, String)
    feistelNetwork block [] = block
    feistelNetwork block (key:keys) = feistelNetwork (feistelRound block key) keys