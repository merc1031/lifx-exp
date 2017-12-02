{-# LANGUAGE RecordWildCards #-}
module Main where

import Lib
import Control.Concurrent
import Control.Concurrent.Async
import Options.Applicative
import Data.Semigroup ((<>))

data Config = Config
  { commandA      :: Command
--  , quiet      :: Bool
--  , enthusiasm :: Int
  }

data Command
  = List
  | Crash

sample :: Parser Config
sample = Config <$>
      -- <$> strOption
      --     ( long "hello"
      --    <> metavar "TARGET"
      --    <> help "Target for the greeting" )
      -- <*> switch
      --     ( long "quiet"
      --    <> short 'q'
      --    <> help "Whether to be quiet" )
      -- <*> option auto
      --     ( long "enthusiasm"
      --    <> help "How enthusiastically to greet"
      --    <> showDefault
      --    <> value 1
      --    <> metavar "INT" )
  subparser
    ( command "list" (info (pure List) ( progDesc "List" ))
   <> command "crash" (info undefined ( progDesc "Crash" ))
    )


main :: IO ()
main = do
  Config {..} <- customExecParser p opts
  r <- mkState
  threadDelay 100000

  cached <- listCached (asSharedState r)
  print cached

  wait $ asReceiveThread r
  wait $ asDiscoveryThread r
  where
    opts = info (sample <**> helper) idm
    p = prefs showHelpOnEmpty

