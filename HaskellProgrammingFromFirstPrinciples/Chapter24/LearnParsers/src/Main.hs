module Main where

import LearnParsers

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "all2:"
  testParseString all2
  pNL "all2':"
  testParseBottom all2'
  pNL "all2'':"
  testParseString all2''
  pNL "all2''':"
  testParseString all2'''
  pNL "any2'':"
  testParseString any2''
  pNL "any2''':"
  testParseString any2'''
  pNL "any2'''':"
  testParseString any2''''
  pNL "any2''''':"
  testParseString any2'''''
  pNL "string':"
  testParseString $ string' "123"
