{-
nickager: does “ (*5) (Sum 5)” work because of “"instance Num a => Num (Sum a)”
nickager: don’t really understand the mechanism here
merijn: nickager: Yes
merijn: :t (*)
lambdabot: Num a => a -> a -> a
merijn: :t Sum 5
lambdabot: Num a => Sum a
merijn: :t 5
lambdabot: Num a => a
merijn: nickager: So it's just inferring that the '5' in (*5) must be "Num a => Sum a", because the second argument of * is "Sum 5 :: Num a => Sum a" and both sides of * need to be identical
nickager: > (* (Sum 5)) (Sum 5)
lambdabot:  Sum {getSum = 25}
nickager: I see
nickager: thanks
merijn: nickager: And the Num instance of Sum and Product are just identical to the instance of their content
merijn: nickager: Exactly to avoid the need to do annoying repeated wrapping/unwrapping
merijn: Better would be to replace the 'a' in all those types to be different, so you get "(*) :: Num a => a -> a -> a", "5 :: Num b => b", "Sum 5 :: Num c => Sum c", GHC then infers "a = Sum c" and "a = b", thus "b = Sum c" 
-}

-- OverloadedStrings
{-
merijn: My biggest objection to OverloadedStrings is that there are many instances floating around that are just plain wrong
merijn: (i.e. that result in runtime error for some strings)
-}