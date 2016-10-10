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
-- @TODO: Modify the types
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
    act :: SteroidAction
  , th  :: CUInt
  , pos :: CUInt
  , pre_proc :: Maybe CUInt 
  , pre_mem  :: Maybe CUInt  
  }
  deriving Show
 
data SteroidEventStruct = SteroidEventStruct
  { 
    act_ :: SteroidAction
  , th_  :: CUInt
  , pos_ :: CUInt
--  , pre_proc_ :: SteroidEventRef
--  , pos_proc_ :: SteroidEventRef
  }
  deriving (Typeable)
type SteroidEventRef = Ptr SteroidEventStruct

instance Storable SteroidEventStruct where
  alignment _ = #{alignment struct stid_event} 
  sizeOf   _ = #{size struct stid_event}
  peek   ptr = do
    act_   <- #{peek struct stid_event, act} ptr
    idxptr <- #{peek struct stid_event, idx} ptr
    th_    <- #{peek struct stid_event, idx.th} idxptr
    pos_   <- #{peek struct stid_event, idx.pos} idxptr
--    pre_proc_ <- #{peek struct stid_event, pre_proc} ptr
--    pre_mem_ <- #{peek struct stid_event, pre_mem} ptr
--    return $ SteroidEvent act_ th_ pos_ pre_proc_ pre_mem_
    return $ SteroidEventStruct act_ th_ pos_ 
  poke   ptr (SteroidEventStruct act_ th_ pos_) = do
    #{poke struct stid_event, act} ptr act_ 

toSteroidEvent :: SteroidEventRef -> IO SteroidEvent 
toSteroidEvent ptr = do
  st <- peek ptr
  has_pre_proc <- stidHasPreProc ptr
  has_pre_mem  <- stidHasPreMem ptr
  pre_proc_ <-
   if has_pre_proc 
   then do 
     pre_proc_ref_ <- stidGetPreProc ptr
     pre_proc_ref <- peek pre_proc_ref_
     return $ Just $ pos_ pre_proc_ref
   else return Nothing
  pre_mem_ <-
   if has_pre_mem 
   then do 
     pre_mem_ref_ <- stidGetPreMem ptr
     pre_mem_ref <- peek pre_mem_ref_
     return $ Just $ pos_ pre_mem_ref
   else return Nothing
  return $ SteroidEvent (act_ st) (th_ st) (pos_ st) pre_proc_ pre_mem_

-- | struct stid_po
data SteroidPo
  deriving (Typeable)
type SteroidPoRef = Ptr SteroidPo

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
  stidGetPoExec :: SteroidRef -> SteroidPoRef -> IO CInt 

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
  stidNewEvent :: SteroidActionRef -> CUInt -> CUInt -> IO SteroidEventRef

foreign import ccall unsafe "stid_set_pre_proc"
  stidSetPreProc :: SteroidEventRef -> SteroidEventRef -> IO CInt

foreign import ccall unsafe "stid_set_pre_mem"
  stidSetPreMem :: SteroidEventRef -> SteroidEventRef -> IO CInt

foreign import ccall unsafe "stid_print_event"
  stidPrintEvent :: SteroidEventRef -> IO CInt

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

foreign import ccall unsafe "stid_get_pre_proc"
  stidGetPreProc :: SteroidEventRef -> IO SteroidEventRef
 
foreign import ccall unsafe "stid_get_pre_mem"
  stidGetPreMem :: SteroidEventRef -> IO SteroidEventRef

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
