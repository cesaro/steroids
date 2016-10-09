{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable, CPP #-}
-------------------------------------------------------------------------------
-- Module    :  Haskroid.DynArr
-- Synopsis  :  Steroid Bindings for Haskell (Dynamic Arrays) 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Haskroid.DynArr where

import Data.Typeable (Typeable)
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt)
import Foreign.Storable

#include <steroid/util/da.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- | Dynamic Arrays
--   Haskell data representation, essentially [a]
data DynArr a = DynArr CInt [a] 
  deriving Show
 
-- | struct da
data DynArrStruct a = DynArrStruct 
  { len :: CInt
  , tab :: Ptr a
  }
  deriving (Typeable)
type DynArrRef a = Ptr (DynArr a)
  
instance Storable a => Storable (DynArrStruct a) where
  alignment _ = #{alignment struct da} 
  sizeOf   _ = #{size struct da}
  peek   ptr = do
    len_ <- #{peek struct da, len} ptr
    tab_ <- #{peek struct da, tab} ptr
    return $ DynArrStruct len_ tab_ 
  poke   ptr (DynArrStruct len_ tab_) = do
    #{poke struct da, len} ptr len_ 
    #{poke struct da, tab} ptr tab_ 

toDynArr :: Storable a => DynArrStruct a -> IO (DynArr a)
toDynArr (DynArrStruct len_ tab_) = do
  table <- peekArray (fromIntegral len_) tab_
  return $ DynArr len_ table

