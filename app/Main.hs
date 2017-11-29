module Main where

import Lib
import Control.Concurrent.Async

main :: IO ()
main = do
  r <- mkState
  wait $ asReceiveThread r
  wait $ asDiscoveryThread r
