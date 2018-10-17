{-# Language ExistentialQuantification #-}

module Main where


-- see: https://chrisdone.com/posts/existentials

data Py = forall a. Show a => Py a

instance Show Py where show (Py s) = show s

instance Show (a -> b) where show _ = "<function>"


main :: IO ()
main = do
    let hetrogenousList = [Py 1,Py 'a',Py "hello",Py (\x -> x ** 2)]
    print hetrogenousList

