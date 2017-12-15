{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import            Control.Concurrent.Async
import            Control.Concurrent.MVar
import            Control.Monad.Reader
import            Data.List                     ( stripPrefix )
import            System.Console.Haskeline      ( InputT
                                                , runInputTBehavior
                                                , useFileHandle
                                                , Behavior
                                                , defaultBehavior
                                                , defaultSettings
                                                , getInputLine
                                                , outputStrLn
                                                )
import            System.Console.Haskeline      ( MonadException )

import            Lib
import            Home.Lights.LIFX.Types
import            Home.Lights.LIFX.Transport

main
  :: IO ()
main
  = do
  r <- mkState

  repState <- newMVar (asSharedState r)
  runRepl repState


  wait $ asReceiveThread r
  wait $ asDiscoveryThread r

-- | Interaction
-- Ask for all lights
--
-- Select a light to act on
--
-- Select a packet to send it
--
--
--
--
--
--
--
--
--
--
--

type ReplState
  = MVar SharedState

process
  :: MonadIO m
  => (String -> ReplT ReplState m ())
  -> String
  -> ReplT ReplState m ()
process o
  = go
  where
    go (stripPrefix ":help" -> Just "") = do
      o ":list"
    go (stripPrefix ":list" -> Just "") = do
      cached <- liftIO $ listCached (asSharedState r)

--      o ":end {timestamp:Integer}"
--      o ":startNow {offsetFromNow:Integer}"
--      o ":endNow {offsetFromNow:Integer}"
--      o ":samples {maxBucketSamples:Integer}"
--      o ":render {avgs|bars|json}"
--      o ":state"
--      o ":grammar {query:String}"
--      o ":type {query:String}"
--      o "Query"


loop
  :: MonadException m
  => (String -> ReplT ReplState m ())
  -> ReplT ReplState m ()
loop o
  = do
  minput <- ReplT $ getInputLine "LIFX> "
  case minput of
    Nothing -> o "Goodbye."
    Just input -> process o input >> loop o



newtype ReplT r m a
  = ReplT { unReplT :: InputT (ReaderT r m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadException)

instance MonadTrans (ReplT s) where
    lift = ReplT . lift . lift

instance Monad m => MonadReader r (ReplT r m) where
    ask = ReplT $ lift ask
    reader r = ReplT $ lift $ reader r


runReplM
  :: MonadException m
  => Behavior
  -> s
  -> ReplT s m a
  -> m a
runReplM b s m
  = runReaderT
  (runInputTBehavior b defaultSettings (unReplT m))
  s



runRepl
  :: MonadException m
  => ReplState
  -> m ()
runRepl st
  = runReplM defaultBehavior st (loop $ ReplT . outputStrLn)

