{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable, CPP, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Haskroid.Haskroid
-- Synopsis  :  Steroid Bindings for Haskell 
-- Copyright :  (c) 2016 Marcelo Sousa
-- Notes:
--   It might be necessary to modify the types
--   to use WordXX.
-------------------------------------------------------------------------------
module Haskroid.Haskroid where

import Data.Typeable (Typeable)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CDouble(..), CInt(..), CUInt(..), CLLong(..), CULLong(..), CSize(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable

import Haskroid.DynArr

#include <steroid/steroid.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- | (opaque) struct stid 
data Steroid
  deriving (Typeable)
type SteroidRef = Ptr Steroid

{- 
 - Replay data structure
 -  Dynamic Array of Context Switches
-}
-- |  struct stid_ctsw
data SteroidCTSW = SteroidCTSW
  { 
    thid :: CUInt
  , nrev :: CUInt
  }
 deriving Show
type SteroidCTSWRef = Ptr SteroidCTSW

instance Storable SteroidCTSW where
  alignment _ = #{alignment struct stid_ctsw} 
  sizeOf   _ = #{size struct stid_ctsw}
  peek   ptr = do
    thid_ <- #{peek struct stid_ctsw, thid} ptr
    nrev_ <- #{peek struct stid_ctsw, nrev} ptr
    return $ SteroidCTSW thid_ nrev_ 
  poke   ptr (SteroidCTSW thid_ nrev_) = do
    #{poke struct stid_ctsw, thid} ptr thid_
    #{poke struct stid_ctsw, nrev} ptr nrev_

-- | Replay data structure
data SteroidReplay = SteroidReplay (DynArr SteroidCTSW)
  deriving Show
 
-- | struct stid_replay
--     struct da tab
data SteroidReplayStruct = SteroidReplayStruct
  {
    tab :: DynArrStruct SteroidCTSW
  } 
type SteroidReplayRef = Ptr SteroidReplayStruct

instance Storable SteroidReplayStruct where
  alignment _ = #{alignment struct stid_replay} 
  sizeOf   _ = #{size struct stid_replay}
  peek   ptr = do
    tab_ <- #{peek struct stid_replay, tab} ptr
    return $ SteroidReplayStruct tab_ 
  poke   ptr (SteroidReplayStruct tab_) = do
    #{poke struct stid_replay, tab} ptr tab_ 

toSteroidReplay :: SteroidReplayStruct -> IO SteroidReplay
toSteroidReplay (SteroidReplayStruct tab_) = do
  table <- toDynArr tab_
  return $ SteroidReplay table
 
{- 
 - Execution data structure
 - Dynamic Array of Actions 
-}
-- | struct stid_action
data SteroidAction = SteroidAction 
  { 
    ty   :: CInt  -- int
  , addr :: CUInt -- uint64_t 
  , val  :: CUInt -- uint64_t
  }
  deriving Show 
type SteroidActionRef = Ptr SteroidAction

instance Storable SteroidAction where
  alignment _ = #{alignment struct stid_action} 
  sizeOf   _ = #{size struct stid_action}
  peek   ptr = do
    ty_   <- #{peek struct stid_action, type} ptr
    addr_ <- #{peek struct stid_action, addr} ptr
    val_  <- #{peek struct stid_action, val} ptr
    return $ SteroidAction ty_ addr_ val_ 
  poke   ptr (SteroidAction ty_ addr_ val_) = do
    #{poke struct stid_action, type} ptr ty_ 
    #{poke struct stid_action, addr} ptr addr_ 
    #{poke struct stid_action, val} ptr val_

-- | struct stid_exec
data SteroidExec
  deriving (Typeable)
type SteroidExecRef = Ptr SteroidExec 

{- 
 - Partial order of events 
 - Event 
-}
-- | struct stid_event
data SteroidEvent = SteroidEvent
  { 
    act_ :: SteroidAction
  , pre_mem_tid_  :: CUInt
  , pre_mem_idx_ :: CUInt
  , sidx_ :: CUInt
  }
  deriving (Typeable, Show)
type SteroidEventRef = Ptr SteroidEvent

instance Storable SteroidEvent where
  alignment _ = #{alignment struct stid_event} 
  sizeOf   _ = #{size struct stid_event}
  peek   ptr = do
    act_   <- #{peek struct stid_event, act} ptr
    tid_    <- #{peek struct stid_event, pre_mem.tid} ptr 
    idx_   <- #{peek struct stid_event, pre_mem.idx} ptr 
    sidx_   <- #{peek struct stid_event, sidx} ptr 
    return $ SteroidEvent act_ tid_ idx_ sidx_ 
  poke   ptr (SteroidEvent act_ tid_ idx_ sidx_) = do
    #{poke struct stid_event, act} ptr act_ 
    #{poke struct stid_event, pre_mem.tid} ptr tid_ 
    #{poke struct stid_event, pre_mem.idx} ptr idx_ 
    #{poke struct stid_event, sidx} ptr sidx_ 

-- | struct stid_po
data SteroidPo = SteroidPo
  { 
    ev_procs :: DynArr (DynArr SteroidEvent)
  , ev_max_lock :: DynArr SteroidEvent
  }
  deriving Show

data SteroidPoStruct = SteroidPoStruct
  {
    procs :: DynArrStruct (DynArrStruct SteroidEvent)
  , max_lock :: DynArrStruct (SteroidEvent) 
  } 
  deriving (Typeable,Show)
type SteroidPoRef = Ptr SteroidPoStruct

instance Storable SteroidPoStruct where
  alignment _ = #{alignment struct stid_po} 
  sizeOf   _ = #{size struct stid_po}
  peek   ptr = do
    procs_ <- #{peek struct stid_po, procs} ptr
    max_lock_ <- #{peek struct stid_po, max_lock} ptr
    return $ SteroidPoStruct procs_ max_lock_
  poke   ptr (SteroidPoStruct procs_ max_lock_) = do
    #{poke struct stid_po, procs} ptr procs_ 
    #{poke struct stid_po, max_lock} ptr max_lock_

toSteroidPo :: SteroidPoStruct -> IO SteroidPo
toSteroidPo (SteroidPoStruct procs max_lock) = do
  _procs <- toDynArr procs
  ev_procs <- case _procs of
    DynArr l x -> do
      y <- mapM toDynArr x
      return $ DynArr l y 
  ev_max_lock <- toDynArr max_lock
  return $ SteroidPo ev_procs ev_max_lock

-- Core API
-- | Should all these calls be unsafe? 
foreign import ccall unsafe "stid_init"  
  stidInit :: IO SteroidRef

foreign import ccall unsafe "stid_term"
  stidTerm :: SteroidRef -> IO CInt

foreign import ccall unsafe "stid_load_bytecode"
  stidLoadBytecode_ :: SteroidRef -> CString -> IO CInt

stidLoadBytecode :: SteroidRef -> String -> IO CInt
stidLoadBytecode ref str = do
  cstr <- newCString str
  stidLoadBytecode_ ref cstr

foreign import ccall unsafe "stid_run"
  stidRun :: SteroidRef -> SteroidReplayRef -> IO CInt

foreign import ccall unsafe "stid_get_seqexec"
  stidGetSeqExec :: SteroidRef -> SteroidExecRef -> IO CInt

foreign import ccall unsafe "stid_get_poexec"
  stidGetPoExec :: SteroidRef -> IO SteroidPoRef 

-- Test API
foreign import ccall unsafe "stid_test"
  stidTest :: IO CInt

-- Actions
foreign import ccall unsafe "stid_new_action"
  stidNewAction :: CInt -> CUInt -> CUInt -> IO SteroidActionRef 

foreign import ccall unsafe "stid_print_action"
  stidPrintAction :: SteroidActionRef -> IO CInt 

-- Context Switches
foreign import ccall unsafe "stid_new_ctsw"
  stidNewCTSW :: CUInt -> CUInt -> IO SteroidCTSWRef

foreign import ccall unsafe "stid_print_ctsw"
  stidPrintCTSW :: SteroidCTSWRef -> IO CInt 

-- Events
foreign import ccall unsafe "stid_new_event"
  stidNewEvent :: SteroidActionRef -> CUInt -> CUInt -> CUInt -> IO SteroidEventRef

foreign import ccall unsafe "stid_print_event"
  stidPrintEvent :: SteroidEventRef -> CUInt -> CUInt -> IO CInt

foreign import ccall unsafe "stid_has_pre_proc"
  stidHasPreProc_ :: SteroidEventRef -> IO CInt

stidHasPreProc :: SteroidEventRef -> IO Bool
stidHasPreProc ref = do
  r <- stidHasPreProc_ ref
  return $ cInt2Bool r

foreign import ccall unsafe "stid_has_pre_mem"
  stidHasPreMem_ :: SteroidEventRef -> IO CInt

stidHasPreMem :: SteroidEventRef -> IO Bool
stidHasPreMem ref = do
  r <- stidHasPreMem_ ref
  return $ cInt2Bool r

-- Partial order
foreign import ccall unsafe "stid_example_po"
  stidExamplePo :: IO SteroidPoRef

foreign import ccall unsafe "stid_print_seq_po"
  stidPrintSeqPo :: SteroidPoRef -> IO ()

-- Replay
foreign import ccall unsafe "stid_get_replay"
  stidGetReplay :: IO SteroidReplayRef 

foreign import ccall unsafe "stid_check_replay"
  stidCheckReplay :: SteroidReplayRef -> IO CInt

cUInt2Bool :: CUInt -> Bool
cUInt2Bool 0 = False 
cUInt2Bool _ = True

cInt2Bool :: CInt -> Bool
cInt2Bool 0 = False
cInt2Bool _ = True 
