{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Lib
import qualified Data.Binary as Bin
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


checkSanity
  :: IO ()
checkSanity = do
  let
    decd :: Either _ (_,_, Header)
    decd = Bin.decodeOrFail ")\NUL\NUL\DC4\210\EOT\NUL\NUL\208s\213%Y\SYN\NUL\NULLIFXV2\SOH\NUL\NUL\241\222i-\238\EOT\NUL\ETX\NUL\NUL\NUL\SOH|\221\NUL\NUL"
  print $ show decd

main :: IO ()
main = do
  Config {..} <- customExecParser p opts
  r <- mkState
  --threadDelay 10000000

  --cached <- listCached (asSharedState r)
  --print cached

  resR <- waitCatch $ asReceiveThread r
  resD <- waitCatch $ asDiscoveryThread r
  print $ "Result of receive " <> show resR
  print $ "Result of discovery " <> show resD
  where
    opts = info (sample <**> helper) idm
    p = prefs showHelpOnEmpty

