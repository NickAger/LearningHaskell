{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE GADTs #-}



-- data C a  =>  T a = Cons a
-- requires DatatypeContext
-- "The designers of Haskell 98 do now think, that it was a bad decision to allow constraints on constructors. GHC as of version 7.2 disallows them by default (turn back on with -XDatatypeContexts).|"
-- see: https://wiki.haskell.org/Data_declaration_with_constraint
ShowBox1 :: forall a. Show a => a -> ShowBox1 a

