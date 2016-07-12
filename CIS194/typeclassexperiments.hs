class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True 


yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = 
    if yesno yesnoVal then yesResult else noResult


-- yesno :: [a] -> Bool
-- yesno [] = False
-- yesno _ = True

-- yesno :: Maybe a -> Bool
-- yesno (Just _) = True
-- yesno Nothing = False