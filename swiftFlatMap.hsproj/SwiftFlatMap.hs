module SwiftFlatMap where
  
import Data.Maybe
  
-- swift flapMap is overloaded but one of the signatures we are interested in is:
-- flatMap<T>(transform: (Self.Generator.Element) -> T?) -> [T]
-- how do we recreate this in Haskell. 

swiftFlatMap :: (a -> b) -> [Maybe a] -> [b]
swiftFlatMap f = (map f) . (map fromJust) . (filter isJust)