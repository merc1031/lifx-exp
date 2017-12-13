module Main where

import Control.Concurrent.Async
import Lib

main
  :: IO ()
main
  = do
  r <- mkState
  wait $ asReceiveThread r
  wait $ asDiscoveryThread r
