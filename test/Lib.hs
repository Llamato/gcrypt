module Main (main) where
    import CryptLib(encryptRotation, decryptRotation, encryptVingenere, decryptVingenere, encryptXorOneTimePad, decryptXorOneTimePad, encryptModOneTimePad, decryptModOneTimePad, padPKCS7, encryptFeistel, decryptFeistel)
    import Test.Hspec (hspec, describe, it, shouldBe)
    import Data.Char (ord, chr)
    
    main :: IO ()
    main =  hspec $ let 
            plaintext = "Broken dreams of glass."
            key = "The quick brown fox jumps over 13 lazy dogs."
            blockSize = 4
        in do 
            describe "rot" $ do
                it "encrypts input by applying a rotation cipher to the input" $ do
                    encryptRotation 6 plaintext `shouldBe` ("Hxuqkt jxkgsy ul mrgyy.")
                it "decrypts input by applying a negative rotation cipher to the input" $ do
                    decryptRotation 6 "Hxuqkt jxkgsy ul mrgyy." `shouldBe` (plaintext)
            
            describe "vingenere" $ do
                it "encrypts input by applying a vigenère cipher to the input" $ do
                    encryptVingenere plaintext key `shouldBe` ("Uys ayvfb fraob kui jmeqj cfie 13 ordy pgux.")
                it "decrypts input by applying the inverse of a vigenère cipher to the input" $ do
                    decryptVingenere plaintext "Uys ayvfb fraob kui jmeqj cfie 13 ordy pgux." `shouldBe` (key)
            
            describe "xoronetimepad" $ do
                it "encrypts input by applying a xor one time pad to the input" $ do
                    encryptXorOneTimePad key plaintext `shouldBe` ([22, 26, 10, 75, 20, 27, 73, 7, 25, 69, 3, 31, 28, 87, 1, 70, 70, 8, 20, 65, 25, 6, 67])
                it "decrypts input by applying a xor one time pad to the input" $ do
                    decryptXorOneTimePad key [22, 26, 10, 75, 20, 27, 73, 7, 25, 69, 3, 31, 28, 87, 1, 70, 70, 8, 20, 65, 25, 6, 67] `shouldBe` (plaintext)
            
            describe "modonetimepad" $ do
                it "encrypts input by applying a modulo one time pad to the input" $ do
                    encryptModOneTimePad key plaintext `shouldBe` "Uyskuh fbebdg bf uiabm."
                it "decrypt input by applying a modulo one time pad to the input" $ do 
                    decryptModOneTimePad key "Uyskuh fbebdg bf uiabm." `shouldBe` (plaintext)

            describe "PKCS7" $ do
                it "pads input by adding bytes until a given block length is reached" $ do 
                    padPKCS7 blockSize "abcd" `shouldBe` "abcd\4\4\4\4"
                    padPKCS7 blockSize "abc" `shouldBe` "abc\1"
                    padPKCS7 blockSize plaintext `shouldBe` plaintext ++ "\1"

            describe "feistel" $ do
                it "encrypts input by piping the input though a feistel network" $ do
                    encryptFeistel key (plaintext) `shouldBe` ([119,34,41,62,56,45,65,45,121,36,23,47,119,47,24,97,116,40,72,33,54,72,89,85])
                it "decrypts input by piping the input though a feistel network" $ do
                    decryptFeistel key [119,34,41,62,56,45,65,45,121,36,23,47,119,47,24,97,116,40,72,33,54,72,89,85] `shouldBe` (plaintext)