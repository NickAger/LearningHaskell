module CaesarCipher (
    caesarCipher
  , uncaesarCipher
  )  where
  
import Data.Char
import CipherDirection

--
masterCaesarCipher :: CipherDirection -> Int -> String -> String
masterCaesarCipher direction n s = map cipherCharacter s
  where
    cipherCharacter ' ' = ' '
    cipherCharacter c = chr ((ord 'A') + ((ord . toUpper $ c) - (ord 'A') `directionOperator` n) `mod` 26)
    directionOperator = case direction of
      Cipher ->  (-)
      UnCipher -> (+)

-- 
caesarCipher ::  Int -> String -> String
caesarCipher = masterCaesarCipher Cipher


uncaesarCipher ::  Int -> String -> String
uncaesarCipher = masterCaesarCipher UnCipher