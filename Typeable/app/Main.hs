{-# LANGUAGE DeriveDataTypeable #-} 

module Main where

import Lib
import Data.Typeable
import Data.Data

data X = X
  deriving (Data,Typeable)


main :: IO ()
main = someFunc
