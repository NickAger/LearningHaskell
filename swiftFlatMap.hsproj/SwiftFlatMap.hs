module SwiftFlatMap where
  
import Data.Maybe
  
-- swift flapMap is overloaded but one of the signatures we are interested in is:
-- flatMap<T>(transform: (Self.Generator.Element) -> T?) -> [T]
-- how do we recreate this in Haskell.
-- UPDATE: August 2018
-- this version of flatMap has now be deprecated in Swift. It is replaced by `compactMap`
-- See: https://github.com/apple/swift-evolution/blob/master/proposals/0187-introduce-filtermap.md

-- UPDATE:
-- Haskell already has `mapMaybe` see: https://www.stackage.org/haddock/lts-12.5/base-4.11.1.0/Data-Maybe.html#v:mapMaybe
-- so the methods below are not necessary though possibly instructive.
-- `mapMaybe f x`  is a shortcut for `catMaybes $ map f x`

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

