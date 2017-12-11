{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where

import Lib
import qualified Data.Binary as Bin
import qualified Data.List as L
import Control.Concurrent
import Control.Concurrent.Async
import Options.Applicative
import Data.Semigroup ((<>))
import Text.Pretty.Simple

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
  threadDelay 3000000

  cached <- listCached (asSharedState r)
  pPrint cached


  let
    Just theaterLamp = L.find (\l ->  lLabel l == Just (Label "Theater Lamp")) cached

  unkP <- newPacket' (asSharedState r) $ \_ p _ o -> print $ "******\nGot: " <> show p <> " " <> show o
  sendToLight (asSharedState r) theaterLamp (unkP $ GetUnknown54)

  ap <- newPacket' (asSharedState r) $ \_ _ _ _ -> pure ()
  sendToLight (asSharedState r) theaterLamp (ap $ SetLightPower (LightPower 65535) (1000))

  let
    loope
      :: Integer
      -> IO ()
    loope !n = do
      let
        Just firstColor = HSBK <$> hue 128.85 <*> saturation 35.8 <*> brightness 100 <*> kelvin 2500
        Just smearColor = HSBK <$> hue 79 <*> saturation 100 <*> brightness 100 <*> kelvin 2500
        Just secondColor = HSBK <$> hue 0.132 <*> saturation 100 <*> brightness 100 <*> kelvin 2500
        firstTransition = 5000
        secondTransition = 5000
        smearTransition = 500
        (c, t) = case n `mod` 4 of
          0 -> (firstColor,  firstTransition)
          1 -> (smearColor,  smearTransition)
          2 -> (secondColor, secondTransition)
          3 -> (smearColor, smearTransition)
      ap' <- newPacket' (asSharedState r) $ \_ _ _ _ -> pure ()
      sendToLight (asSharedState r) theaterLamp (ap' $ SetColor () c t)
      threadDelay $ fromIntegral $ t * 1000
      loope (n + 1)

  loope 0


  resR <- waitCatch $ asReceiveThread r
  resD <- waitCatch $ asDiscoveryThread r
  print $ "Result of receive " <> show resR
  print $ "Result of discovery " <> show resD
  where
    opts = info (sample <**> helper) idm
    p = prefs showHelpOnEmpty

