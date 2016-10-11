{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Hapiroid
-- Synopsis  :  Haskell API for Steroid
-- Copyright :  (c) 2016 Marcelo Sousa
-- 
-------------------------------------------------------------------------------
module Haskroid.Hapiroid where

import Haskroid.Haskroid
import Haskroid.DynArr

data Action = Act 
 { 
   act_ty   :: Int
 , act_addr :: Integer
 , act_val  :: Integer
 }
 deriving Show

data Event = Ev
 {
   ev_act :: Action          -- ^ action
-- , ev_tid :: Integer         -- ^ thread of ev
-- , ev_idx :: Integer         -- ^ index in thread 
 , ev_pre_mem_tid :: Integer -- ^ thread of mem pre
 , ev_pre_mem_idx :: Integer -- ^ index in thread of mem pre
 , ev_sidx :: Integer        -- ^ index in the stream
 }
 deriving Show

data Poset = Poset
 {
   evs_procs :: [[Event]] 
 , evs_max_lock :: [Event]
 }
 deriving Show

-- | Convert DynArr to List
toList :: (a -> b) -> DynArr a -> [b]
toList f (DynArr _ la) = map f la

-- | Convert to pure poset 
toPoset :: SteroidPo -> Poset
toPoset p@SteroidPo{..} = 
  let evs = toList (toList toEvent) ev_procs
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
  let act_ty = fromInteger $ toInteger ty 
      act_addr = toInteger addr
      act_val = toInteger val 
  in Act act_ty act_addr act_val 

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
show_evs_proc tid es = showS_evs_proc tid (zip es [0..]) ""
 where
  showS_evs_proc tid es s = 
    case es of
      [] -> s
      ((e,i):ee) ->
        let se = show_event tid i e
        in showS_evs_proc tid ee (s ++ se) 

sep = "\n-------------------\n"

show_event :: Int -> Int -> Event -> String
show_event tid i e@Ev{..} = 
  let s1 = "Event: tid = " ++ show tid ++ ", pos = " ++ show i ++ ", id = " ++ show ev_sidx 
      s2 = show ev_act
      s3 = "Memory predecessor: tid = " ++ show ev_pre_mem_tid ++ ", pos = " ++ show ev_pre_mem_idx
  in sep ++ s1 ++ "\n" ++ s2 ++ "\n" ++ s3 


