module Main (main) where
    import CryptLib(encryptRotation, decryptRotation, encryptVingenere, decryptVingenere, encryptOneTimePad, decryptOneTimePad)
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
        
        {- describe "onetimepad" $ do
            it "encrypts input by applying a xor one time padding with a key to the input" $ do
                encryptOneTimePad "Broken dreams of glass" "The quick Brown fox jumps over the lazy dog" `shouldBe` ("UYSAY VFBFR AOBKU IJME")
            it "decrypts input by apply a xor one time padding with a key to the input" $ do
                decryptOneTimePad "UYSAY VFBFR AOBKU IJME" "The quick Brown fox jumps over the lazy dog" `shouldBe` ("Broken dreams of glass") -}