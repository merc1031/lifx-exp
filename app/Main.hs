module Main where

import            Control.Concurrent.Async

import            Lib
import            Home.Lights.LIFX.Types

main
  :: IO ()
main
  = do
  r <- mkState
  wait $ asReceiveThread r
  wait $ asDiscoveryThread r
