{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Trifecta
import Test.Hspec
import qualified Data.Map as M
import DataIni

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
      let m = parseByteString parseAssignment mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")
  describe "Header Parsing" $
    it "can parse a simple header" $ do
      let m = parseByteString parseHeader mempty headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")
  describe "Comment Parsing" $
    it "Can skip a comment before a header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")
  describe "Section Parsing" $
    it "Can parse a simple section" $ do
      let m = parseByteString parseSection mempty sectionEx
          r' = maybeSuccess m
          states = M.fromList [("Chris", "Texas")]
          expected' = Just (Section
                            (Header "states")
                            states)
      print m
      r' `shouldBe` expected'
  describe "INI Parsing" $
    it "Can parse multiple sections" $ do
      let m = parseByteString parseIni mempty sectionEx''
          r' = maybeSuccess m
          sectionValues = M.fromList
                          [ ("alias", "claw"), ("host", "wikipedia.org")]
          whatisitValues = M.fromList
                            [("red", "intoothandclaw")]
          expected' = Just (Config
                            (M.fromList
                             [ (Header "section", sectionValues)
                             , (Header "whatisit", whatisitValues)]))
      print m
      r' `shouldBe` expected'




--  print $ parseByteString parseSection mempty sectionEx
--  print $ parseByteString parseIni mempty sectionEx
