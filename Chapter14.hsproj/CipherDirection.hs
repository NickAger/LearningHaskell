module CipherDirection (
    CipherDirection(UnCipher, Cipher)
  ) where
  
data CipherDirection = UnCipher | Cipher deriving (Show, Eq)