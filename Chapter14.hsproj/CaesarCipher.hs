module CaesarCipher (
  caesarCipher,
  uncaesarCipher
  )  where
  
import Data.Char

caesarCipher ::  Int -> String -> String
caesarCipher n s = map encryptCharacter s
  where
    encryptCharacter ' ' = ' '
    encryptCharacter c = chr ((ord 'A') + ((ord . toUpper $ c) - (ord 'A') + n) `mod` 26)


uncaesarCipher ::  Int -> String -> String
uncaesarCipher n s = map decryptCharacter s 
  where
     decryptCharacter ' ' = ' '
     decryptCharacter c =  chr ((ord 'A') + ((ord . toUpper $ c) - (ord 'A') - n) `mod` 26)