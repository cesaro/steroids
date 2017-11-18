{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Hapiroid
-- Synopsis  :  Haskell API for Steroid
-- Copyright :  (c) 2016 Marcelo Sousa
-- 
-------------------------------------------------------------------------------
module Haskroid.Hapiroid where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.Storable
import Haskroid.DynArr
import Haskroid.Haskroid

data ActType =
    WR     
  | RD     
  | LOCK   
  | UNLOCK 
  | CREATE 
  | JOIN   
  | CTSW   
  | EXIT   
  | ENTRY  
  | MALLOC 
  | FREE   
  | ALLOCA 
  | CALL   
  | RET    
  deriving (Show, Enum, Ord, Eq)

data Action = Act 
 { 
   act_ty   :: ActType 
 , act_addr :: Integer
 , act_val  :: Integer
 }
  deriving (Show, Ord, Eq)

data Event = Ev
 {
   ev_act         :: Action  -- ^ action
-- , ev_tid         :: Integer -- ^ thread of ev
-- , ev_idx         :: Integer -- ^ index in thread 
 , ev_pre_mem_tid :: Integer -- ^ thread of mem pre
 , ev_pre_mem_idx :: Integer -- ^ index in thread of mem pre
 , ev_sidx        :: Integer -- ^ index in the stream
 }
  deriving (Show, Ord, Eq)

data Poset = Poset
 {
   evs_procs    :: [[Event]] 
 , evs_max_lock :: [Event]
 }
  deriving Show

type Replay = [SteroidCTSW]

-- | API
-- Start and Load an LLVM bytecode file
start_and_load :: FilePath -> IO SteroidRef
start_and_load file = do
  stid <- stidInit
  rLoad <- stidLoadBytecode stid file
  if rLoad == 0
  then return stid
  else error $ "haskroid: start_and_load: error loading " ++ file 

-- Terminate the steroid engine
terminate :: SteroidRef -> IO ()
terminate ptr = do
  rTerm <- stidTerm ptr
  if rTerm == 0
  then return ()
  else error $ "haskroid: terminate: fatal error code " ++ show rTerm

-- Get a partial order in free mode
run_free :: SteroidRef -> IO Poset 
run_free stid = do
  rRun <- stidRun stid nullPtr
  if rRun == 0
  then do
    poPtr        <- stidGetPoExec stid
    hs_po_struct <- peek poPtr
    hs_po        <- toSteroidPo hs_po_struct
    return $! toPoset hs_po
  else error $ "run_free: error code " ++ show rRun

-- Send the replay down and get a new partial order
replay :: SteroidRef -> Replay -> Int -> IO Poset
replay stid rep len_rep = do
  -- print $ "low_level replay: " ++ show (rep, len_rep)
  _ <- allocaArray len_rep (\tab_ -> do
    _ <- pokeArray tab_ rep
    let da = DynArrStruct (fromIntegral len_rep) tab_ 
        rep_struct = SteroidReplayStruct da
    -- print $ "low_level replay: rep_struct " ++ show rep_struct 
    _ <- alloca (\rep_ptr -> do
      _ <- poke rep_ptr rep_struct
      rRun <- stidRun stid rep_ptr
      if rRun == 0
      then return ()
      else error $ "replay: run error code " ++ show rRun)
    return ())
  poPtr        <- stidGetPoExec stid
  hs_po_struct <- peek poPtr
  hs_po        <- toSteroidPo hs_po_struct
  return $! toPoset hs_po
 
-- | Converters  
-- | Convert DynArr to List
toList :: (a -> b) -> DynArr a -> [b]
toList f (DynArr _ la) = map f la

-- | Convert to pure poset 
toPoset :: SteroidPo -> Poset
toPoset p@SteroidPo{..} = 
  let evs   = toList (toList toEvent) ev_procs
      locks = toList toEvent ev_max_lock
  in Poset evs locks

toEvent :: SteroidEvent -> Event
toEvent e@SteroidEvent{..} =
  let ev_act = toAction act_
      ev_pre_mem_tid = toInteger pre_mem_tid_
      ev_pre_mem_idx = toInteger pre_mem_idx_
      ev_sidx = toInteger sidx_
  in Ev ev_act ev_pre_mem_tid ev_pre_mem_idx ev_sidx  

toAction :: SteroidAction -> Action
toAction a@SteroidAction{..} =
  let act_ty = toEnum $ fromInteger $ toInteger ty 
      act_addr = toInteger addr
      act_val = toInteger val 
  in Act act_ty act_addr act_val 

-- | Pretty Printers
show_poset_simple :: Poset -> String
show_poset_simple p@Poset{..} =
  let evs_s = show_evs_per_proc evs_procs
  --    lock_s = show_evs_proc (-1) evs_max_lock
      s1 = "Begin events\n------------------\n"
  in s1 ++ evs_s -- ++ lock_s

show_evs_per_proc :: [[Event]] -> String
show_evs_per_proc evs_procs =
  let list = zip evs_procs [0..]
      strs = map (\(es,tid) -> show_evs_proc tid es) list
  in unlines strs 
 
show_evs_proc :: Int -> [Event] -> String
show_evs_proc tid es = showS_evs_proc (zip es [0..]) ""
 where
  showS_evs_proc evs s = 
    case evs of
      [] -> s
      ((e,i):ee) ->
        let se = show_event tid i e
        in showS_evs_proc ee (s ++ se) 

show_event :: Int -> Int -> Event -> String
show_event tid i e@Ev{..} = 
  let s1 = "eventt   tid  " ++ show tid ++ " pos  " ++ show i ++ " sidx  " ++ show ev_sidx 
      s2 = " ac.type  " ++ show (act_ty ev_act)
      s3 = " pre_mem { tid  " ++ show ev_pre_mem_tid ++ " idx " ++ show ev_pre_mem_idx
  in s1 ++ s2 ++ s3 ++ " } \n"

