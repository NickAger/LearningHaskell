module Identity where
  
import Control.Applicative

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
  
