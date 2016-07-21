module SwiftFlatMap where
  
import Data.Maybe
  
-- swift flapMap is overloaded but one of the signatures we are interested in is:
-- flatMap<T>(transform: (Self.Generator.Element) -> T?) -> [T]
-- how do we recreate this in Haskell. 

swiftFlatMap :: (a -> b) -> [Maybe a] -> [b]
swiftFlatMap f = (map f) . (map fromJust) . (filter isJust)

-- alternative 1
swiftFlatMap2 :: (a -> b) -> [Maybe a] -> [b]
swiftFlatMap2 f = (map f) . catMaybes

-- alternative 2
swiftFlatMap3 :: (a -> b) -> [Maybe a] -> [b]
swiftFlatMap3 f = (map f) . (foldr filterAndExtract [])
  where
    filterAndExtract Nothing xs = xs
    filterAndExtract (Just x) xs = x:xs

