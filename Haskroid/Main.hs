{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable, CPP #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Synopsis  :  Steroid Bindings for Haskell 
-- Copyright :  (c) 2016 Marcelo Sousa
-- This module is for testing purposes
-------------------------------------------------------------------------------
module Main where

import Foreign.Ptr 
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Haskroid.Haskroid
import Haskroid.DynArr

main = testPrintCTSW 

test1 :: IO ()
test1 = do
  str <- stidInit
  t <- stidLoadBytecode str "/tmp/main.bc"
  print t
  c <- stidTerm str
  print c

test2 :: IO ()
test2 = do
  s <- stidInit
  load_ret <- stidLoadBytecode s "/tmp/main.bc"
  run_ret <- stidRun s nullPtr
  stidGetSeqExec s undefined -- need a pointer to the execution
  print run_ret

test3 :: IO ()
test3 = do
  s <- stidTest 
  print s

-- | Test if we can correctly read a Steroid Action
testNewAction :: IO ()
testNewAction = do
  ptr <- stidNewAction 0 255 5
  act <- peek ptr
  print act 

-- | Test if we can correctly print a Steroid Action
testPrintAction :: IO ()
testPrintAction = do
  ptr <- stidNewAction 0 255 5
  ret <- stidPrintAction ptr
  print ret
  act <- peek ptr
  print act 
  ret2 <- alloca (\ptr -> do
    pa <- poke ptr act
    stidPrintAction ptr)
  print ret2

-- | Test if we can correctly read a Steroid CTSW 
testNewCTSW :: IO ()
testNewCTSW = do
  ptr <- stidNewCTSW 1 2 
  act <- peek ptr
  print act 

-- | Test if we can correctly print a Steroid CTSW
testPrintCTSW :: IO ()
testPrintCTSW = do
  ptr <- stidNewCTSW 1 2
  act <- peek ptr
  print act 
  ret2 <- alloca (\ptr -> do
    pa <- poke ptr act
    stidPrintCTSW ptr)
  print ret2

-- | Test if we can get and send a replay
testGetSendReplay :: IO ()
testGetSendReplay = do
  rep <- stidGetReplay
  ret <- stidCheckReplay rep
  print "testing..."
  hs_rep_struct <- peek rep
  hs_rep <- toSteroidReplay hs_rep_struct
  print hs_rep

-- | Test if we can create a replay and send it down
testReplay :: IO ()
testReplay = do
  ret <- allocaArray 2 (\tab_ -> do
    -- create the pointer to the table
    _ <- pokeArray tab_ rep
    -- da :: DynArrStruct ...
    let da = DynArrStruct 2 tab_
        rep = SteroidReplayStruct da 
    ret2 <- alloca (\rep_ptr -> do
      _ <- poke rep_ptr rep
      stidCheckReplay rep_ptr)
    return ret2) 
  print ret

ctx1, ctx2 :: SteroidCTSW
ctx1 = SteroidCTSW 5 10
ctx2 = SteroidCTSW 7 76 

rep :: [SteroidCTSW]
rep = [ctx1,ctx2]

