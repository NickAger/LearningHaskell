module TestCiphers where
  
import CaesarCipher
import VigenereCipher
import Test.QuickCheck
import Data.Char (toUpper)

-- from http://stackoverflow.com/questions/20934506/haskell-quickcheck-how-to-generate-only-printable-strings
genUpperChar :: Gen Char
genUpperChar = elements $ ' ' : ['A'..'Z']

genUpperString :: Gen String
genUpperString = listOf genUpperChar

newtype UpperString = UpperString { unwrapUpperString :: String }
    deriving Show

-- ensure we don't generate empty strings; from NonEmptyList
instance Arbitrary UpperString where
    arbitrary = UpperString <$> (genUpperString `suchThat` (not . null))
--

prop_caesarCipher :: Int -> UpperString -> Bool
prop_caesarCipher seed (UpperString target) = (uncaesarCipher seed $ caesarCipher seed target) == target

prop_vigenèreCipher :: UpperString ->  UpperString -> Bool
prop_vigenèreCipher (UpperString seed) (UpperString target) = (vigenèreUnCipher seed $ vigenèreCipher seed target) == target