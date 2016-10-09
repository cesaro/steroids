{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable, CPP #-}
-------------------------------------------------------------------------------
-- Module    :  Haskroid.Haskroid
-- Synopsis  :  Steroid Bindings for Haskell 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Haskroid.Haskroid where

import Control.Applicative ((<$>), (<*>))
import Data.Typeable (Typeable)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CDouble(..), CInt(..), CUInt(..), CLLong(..), CULLong(..), CSize(..))
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable

#include <steroid/steroid.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- | (opaque) struct stid 
data Steroid
  deriving (Typeable)
type SteroidRef = Ptr Steroid

-- |  struct stid_ctsw
--   @TODO: Use Integer instead of Int 
data SteroidCTSW = SteroidCTSW
  { thid :: Int
  , nrev :: Int
  }
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

-- | struct stid_replay
--     struct da tab
data SteroidReplay
  deriving (Typeable)
type SteroidReplayRef = Ptr SteroidReplay

-- | struct stid_action
-- @TODO: Modify the types
data SteroidAction = SteroidAction 
  { ty   :: Int -- int
  , addr :: Int -- size_t
  , val  :: Int -- uint64_t
  } 
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
  
-- | struct stid_event
data SteroidEvent
  deriving (Typeable)
type SteroidEventRef = Ptr SteroidEvent

-- | struct stid_po
data SteroidPo
  deriving (Typeable)
type SteroidPoRef = Ptr SteroidPo

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

foreign import ccall unsafe "stid_test"
  stidTest :: IO CInt 
