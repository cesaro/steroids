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

-- | (opaque) struct steroid 
data Steroid
  deriving (Typeable)
type SteroidRef = Ptr Steroid

-- |  struct steroid_ctsw
--   @TODO: Use Integer instead of Int 
data SteroidCTSW = SteroidCTSW
  { thid :: Int
  , nrev :: Int
  }
type SteroidCTSWRef = Ptr SteroidCTSW

instance Storable SteroidCTSW where
  alignment _ = #{alignment struct steroid_ctsw} 
  sizeOf   _ = #{size struct steroid_ctsw}
  peek   ptr = do
    thid_ <- #{peek struct steroid_ctsw, thid} ptr
    nrev_ <- #{peek struct steroid_ctsw, nrev} ptr
    return $ SteroidCTSW thid_ nrev_ 
  poke   ptr (SteroidCTSW thid_ nrev_) = do
    #{poke struct steroid_ctsw, thid} ptr thid_
    #{poke struct steroid_ctsw, nrev} ptr nrev_

-- | struct steroid_replay
--     struct da tab
data SteroidReplay
  deriving (Typeable)
type SteroidReplayRef = Ptr SteroidReplay

-- | struct steroid_action
-- @TODO: Modify the types
data SteroidAction = SteroidAction 
  { ty   :: Int -- int
  , addr :: Int -- size_t
  , val  :: Int -- uint64_t
  } 
type SteroidActionRef = Ptr SteroidAction

instance Storable SteroidAction where
  alignment _ = #{alignment struct steroid_action} 
  sizeOf   _ = #{size struct steroid_action}
  peek   ptr = do
    ty_   <- #{peek struct steroid_action, type} ptr
    addr_ <- #{peek struct steroid_action, addr} ptr
    val_  <- #{peek struct steroid_action, val} ptr
    return $ SteroidAction ty_ addr_ val_ 
  poke   ptr (SteroidAction ty_ addr_ val_) = do
    #{poke struct steroid_action, type} ptr ty_ 
    #{poke struct steroid_action, addr} ptr val_ 
    #{poke struct steroid_action, val} ptr val_

-- | struct steroid_exec
data SteroidExec
  deriving (Typeable)
type SteroidExecRef = Ptr SteroidExec 
  
-- | struct steroid_event
data SteroidEvent
  deriving (Typeable)
type SteroidEventRef = Ptr SteroidEvent

-- | struct steroid_po
data SteroidPo
  deriving (Typeable)
type SteroidPoRef = Ptr SteroidPo

-- | Should all these calls be unsafe? 
foreign import ccall unsafe "steroid_init"  
  steroidInit :: IO SteroidRef

foreign import ccall unsafe "steroid_term"
  steroidTerm :: SteroidRef -> IO CInt

foreign import ccall unsafe "steroid_load_bytecode"
  steroidLoadBytecode_ :: SteroidRef -> CString -> IO CInt

steroidLoadBytecode :: SteroidRef -> String -> IO CInt
steroidLoadBytecode ref str = do
  cstr <- newCString str
  steroidLoadBytecode_ ref cstr

foreign import ccall unsafe "steroid_run"
  steroidRun :: SteroidRef -> SteroidReplayRef -> IO CInt

foreign import ccall unsafe "steroid_get_seqexec"
  steroidGetSeqExec :: SteroidRef -> SteroidExecRef -> IO CInt

foreign import ccall unsafe "steroid_get_poexec"
  steroidGetPoExec :: SteroidRef -> SteroidPoRef -> IO CInt 

