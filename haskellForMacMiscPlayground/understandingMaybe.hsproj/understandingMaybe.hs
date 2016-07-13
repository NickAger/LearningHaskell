testMaybe :: Maybe Int -> Maybe String
testMaybe (Just x) = Just (show x)
testMaybe Nothing = Nothing