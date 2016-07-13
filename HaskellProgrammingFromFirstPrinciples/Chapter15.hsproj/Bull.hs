module Bull where
  
import Control.Monad
import Data.Monoid
import Test.QuickCheck
import MonoidLaws

data Bull = 
    Fools
  | Twoo
  deriving (Eq, Show)
  
instance Arbitrary Bull where
  arbitrary =
    frequency [(1, return Fools)
              ,(1, return Twoo) ]
              

instance Monoid Bull where
   mempty = Fools
   mappend _ _ = Fools
   
type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)
