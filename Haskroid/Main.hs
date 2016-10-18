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

import Haskroid.Hapiroid
import Haskroid.Haskroid
import Haskroid.DynArr

main = realTest 

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

-- | Test if we can get an event
testPrintEvent :: IO ()
testPrintEvent = do
  aptr <- stidNewAction 0 255 5
--  ret' <- stidPrintAction aptr
--  print ret'
  evt  <- stidNewEvent aptr 1 3 1
  print "testPrintEvent"
  ret  <- stidPrintEvent evt 1 1
  print ret
  ev <- peek evt
  print ev
  print "done"

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

-- | Test if we can get a partial order
testGetPartialOrder :: IO ()
testGetPartialOrder = do
  ptr <- stidExamplePo
  hs_po_struct <- peek ptr
  hs_po <- toSteroidPo hs_po_struct
  print hs_po

-- | Test if we can get and print partial order
testGetPrintPartialOrder :: IO ()
testGetPrintPartialOrder = do
  ptr <- stidExamplePo
  stidPrintSeqPo ptr

testPoset :: IO ()
testPoset = do 
  ptr <- stidExamplePo
  hs_po_struct <- peek ptr
  hs_po <- toSteroidPo hs_po_struct
  let po = toPoset hs_po 
  putStrLn $ show_poset_simple po


realTest :: IO ()
realTest = do
  stid <- stidInit
  rLoad <- stidLoadBytecode stid "input.ll"
  print $ "HASKELL: " ++ show  rLoad
  -- stidRun stid nullPtr
  -- print $ "HASKELL: RUN COMPLETED"
  poPtr <- stidGetPoExec stid
  stidPrintSeqPo poPtr 
  print $ "HASKELL: GET PO"
  hs_po_struct <- peek poPtr
--  print hs_po_struct 
  hs_po <- toSteroidPo hs_po_struct
  let po = toPoset hs_po 
  putStrLn $ show_poset_simple po
  c <- stidTerm stid
  print c
