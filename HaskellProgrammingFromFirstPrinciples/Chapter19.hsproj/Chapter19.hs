import Data.Monoid
import qualified Data.Map as M
import Data.Map

f' = const (Sum 1)
g' = const (Sum 2)

y x = Sum x
z x = Sum (x * x)
