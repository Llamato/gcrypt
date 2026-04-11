{-# LANGUAGE BinaryLiterals #-}
module CryptLib.Internal (casedAlphabet, rtlen, removeDirt, readdDirt, cdiv, pairToList, swapPair, halves, padBlock, unpadBlock, trimBits, feistelRoundKeys, feistelNetwork) where
    import Data.Char (isLowerCase)
    import Data.List (findIndex, find)
    import Data.List.Split (chunksOf)
    import Data.Maybe (isNothing)
    import Data.Char (ord, chr)
    import Data.Bits ((.&.), (.|.), xor, shiftL)
    
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
    padBlock blockSize block
        | length block `mod` blockSize == 0 = block
        | otherwise = block ++ replicate ((blockSize - length block) `mod` blockSize) '\NUL'

    unpadBlock :: Int -> String -> String
    unpadBlock blockSize block
        | length block `mod` blockSize == 0 = block
        | otherwise = take ((length block `div` blockSize) * blockSize) block

    strxor :: String -> String -> String
    strxor str1 str2 = map chr $ zipWith xor (map ord str1) (map ord str2)

    trimBits :: Int -> String -> String
    trimBits bsize str = init str ++ [chr (ord lastByte .&. mask)]
        where 
            lastByte = last $ take (bsize `div` 8) str
            bitsToKeep = bsize `mod` 8
            mask = (1 `shiftL` bitsToKeep) - 1

    feistelRoundKeys :: String -> Int -> Int -> [String]
    feistelRoundKeys key blockSizechars rounds = map (padBlock halfblockSize) (take rounds $ cycle $ chunksOf halfblockSize key)
        where 
            halfblockSize = blockSizechars `div` 2

    feistelRound :: (String, String) -> String -> (String, String)
    feistelRound (l1, r1) k1 = (l2, r2)
        where 
            f1 = r1 `strxor` k1
            r2 = l1 `strxor` f1
            l2 = r1

    feistelNetwork :: (String, String) -> [String] -> (String, String)
    feistelNetwork block [] = block
    feistelNetwork block (key:keys) = feistelNetwork (feistelRound block key) keys

    lookupSubstitution :: Char -> [(Char, Char)] -> Char
    lookupSubstitution input lookupTable = case find (\c -> input==fst c) lookupTable of
        Just lookupEntry -> snd lookupEntry
        Nothing -> input