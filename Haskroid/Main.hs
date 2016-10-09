{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable, CPP #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Synopsis  :  Steroid Bindings for Haskell 
-- Copyright :  (c) 2016 Marcelo Sousa
-- This module is for testing purposes
-------------------------------------------------------------------------------
module Main where

import Foreign.Ptr 
import Haskroid.Haskroid

-- main :: IO ()
-- main = do
--   str <- stidInit
--   t <- stidLoadBytecode str "/tmp/main.bc"
--   print t
--   c <- stidTerm str
--   print c


test1 :: IO ()
test1 = do
  s <- stidInit
  load_ret <- stidLoadBytecode s "/tmp/main.bc"
  run_ret <- stidRun s nullPtr
  stidGetSeqExec s undefined -- need a pointer to the execution
  print run_ret

main :: IO ()
main = do
  s <- stidTest 
  print s
