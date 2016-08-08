{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Marshalling

main :: IO ()
main = do
  let d = decode sectionJson :: Maybe TestData
  print d
