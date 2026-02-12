module Main (main) where
    import CryptLib(encryptRotation, decryptRotation, encryptVingenere, decryptVingenere, encryptOneTimePad, decryptOneTimePad, encryptFeistel, decryptFeistel)
    import Test.Hspec (hspec, describe, it, shouldBe)
    
    main :: IO ()
    main =  hspec $ do 
        describe "rot" $ do
            it "encrypts input by applying a rotation cipher to the input" $ do
                encryptRotation 6 "Broken dreams of glass." `shouldBe` ("Hxuqkt jxkgsy ul mrgyy.")
            it "decrypts input by applying a negative rotation cipher to the input" $ do
                decryptRotation 6 "Hxuqkt jxkgsy ul mrgyy." `shouldBe` ("Broken dreams of glass.")
        
        describe "vingenere" $ do
            it "encrypts input by applying a vigenère cipher to the input" $ do
                encryptVingenere "Broken dreams of glass" "The quick brown fox jumps over 13 lazy dogs." `shouldBe` ("Uys ayvfb fraob kui jmeqj cfie 13 ordy pgux.")
            it "decrypts input by applying the inverse of a vigenère cipher to the input" $ do
                decryptVingenere "Broken dreams of glass" "Uys ayvfb fraob kui jmeqj cfie 13 ordy pgux." `shouldBe` ("The quick brown fox jumps over 13 lazy dogs.")
        
        describe "onetimepad" $ do
            it "encrypts input by applying a xor one time pad to the input" $ do
                encryptOneTimePad "The quick brown fox jumps over 13 lazy dogs." "Broken dreams of glass" `shouldBe` ([22, 26, 10, 75, 20, 27, 73, 7, 25, 69, 3, 31, 28, 87, 1, 70, 70, 8, 20, 65, 25, 6])
            it "decrypts input by applying a xor one time pad to the input" $ do
                decryptOneTimePad "The quick brown fox jumps over 13 lazy dogs." [22, 26, 10, 75, 20, 27, 73, 7, 25, 69, 3, 31, 28, 87, 1, 70, 70, 8, 20, 65, 25, 6] `shouldBe` ("Broken dreams of glass")
        
        describe "feistel" $ do
            it "encrypts input by piping the input though a feistel network" $ do
                encryptFeistel "The quick brown fox jumps over the 13 lazy dogs." "Broken dreams of glass" `shouldBe` ([119, 34, 41, 123, 56, 45, 65, 104, 121, 36, 23, 106, 119, 47, 24, 36, 116, 40, 72, 100, 107, 4])
            it "decrypts input by piping the input though a feistel network" $ do
                decryptFeistel "The quick brown fox jumps over the 13 lazy dogs." [119, 34, 41, 123, 56, 45, 65, 104, 121, 36, 23, 106, 119, 47, 24, 36, 116, 40, 72, 100, 107, 4] `shouldBe` ("Broken dreams of glass")