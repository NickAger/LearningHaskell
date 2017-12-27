-- see also: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
-- THE READERT DESIGN PATTERN

-- from http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html#1

import Control.Monad.Reader
import Data.Map
import qualified Data.Map as Map
import Data.Maybe


type Bindings = Map String Int;

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calc_isCountCorrect bindings

-- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: Reader Bindings Bool
calc_isCountCorrect = do
    count <- asks (lookupVar "count")
    bindings <- ask
    return (count == (Map.size bindings))

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = fromJust (Map.lookup name bindings)

sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]

main = do
    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": ";
    putStrLn $ show (isCountCorrect sampleBindings);
