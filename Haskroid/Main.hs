{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable, CPP #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Synopsis  :  Steroid Bindings for Haskell 
-- Copyright :  (c) 2016 Marcelo Sousa
-- This module is for testing purposes
-------------------------------------------------------------------------------
module Main where

import Haskroid.Haskroid

main :: IO ()
main = do
  str <- steroidInit
  t <- steroidLoadBytecode str "/tmp/main.bc"
  print t
  c <- steroidTerm str
  print c

