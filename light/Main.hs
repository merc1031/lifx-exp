{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where

import Lib
import qualified Data.Binary as Bin
import qualified Data.List as L
import Control.Monad
import            Control.Monad.Except
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Default
import Data.Word
import Options.Applicative
import Data.Semigroup ((<>))
import Network.Info (getNetworkInterfaces)
import qualified Network.Info as NI
import Text.Pretty.Simple
import            Network.Socket                ( Socket (..)
                                                , SockAddr (..)
                                                , tupleToHostAddress
                                                , hostAddressToTuple
                                                , SocketOption (..)
                                                , setSocketOption
                                                , isSupportedSocketOption
                                                , bind
                                                , defaultProtocol
                                                , aNY_PORT
                                                , socket
                                                , Family(AF_INET)
                                                , SocketType(Datagram)
                                                , PortNumber
                                                )
import Network.Socket.ByteString
import qualified Data.ByteString.Lazy as BSL

data MorphedError
  = MorphedPayloadError PayloadDecodeError
  | UnknownPacketError String
  deriving Show

lightReceiveThread
  :: NI.NetworkInterface
  -> Socket
  -> TVar FakeBulb
  -> IO (Async ())
lightReceiveThread nic ss bulbM
  = async $ forever $ do
  print "About to recv"
  (!bs, sa) <- recvFrom ss 1500
  let
    bsl = BSL.fromStrict bs
    headerE = runExcept $ decodeHeader Nothing bsl

  encoded <- forM headerE $ \(header, rest) -> do
    let
      sequ = faSequence $ hFrameAddress header
      ackR = faAckRequired $ hFrameAddress header
      uniqS = fSource $ hFrame header
      msgT = phType $ hProtocolHeader header
      nicToTarget = case NI.mac nic of
        NI.MAC b1 b2 b3 b4 b5 b6 -> Target $ Mac $ (0xd0, 0x73, 0x8f, 0x86, 0xbf, 0xaf)

    when (ackR == AckRequired) $ do
      let
        bytes = Bin.encode packet
        packet = mkTestPacket
          SingleTagged
          uniqS
          nicToTarget
          NoAckRequired
          NoResRequired
          sequ
          (Reply $ DeviceReplyType AcknowledgementReply)
          (Acknowledgement)

      sendManyTo ss (BSL.toChunks bytes) sa

    case msgT of
      (Request (DeviceMessageType GetServiceMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetService header rest
        forM payloadE $ \payload -> do
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ DeviceReplyType StateServiceReply)
              (StateService 1 56700)

          pure $ (BSL.toChunks bytes)
      (Request (DeviceMessageType GetHostFirmwareMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetHostFirmware header rest
        forM payloadE $ \payload -> do
          FakeBulbFirmware {..} <- atomically $ fbFirmware <$> readTVar bulbM
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ DeviceReplyType StateHostFirmwareReply)
              (StateHostFirmware fbfBuild 0 fbfVersion)

          pure $ (BSL.toChunks bytes)
      (Request (DeviceMessageType GetWifiFirmwareMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetWifiFirmware header rest
        forM payloadE $ \payload -> do
          FakeBulbWifiFirmware {..} <- atomically $ fbWifiFirmware <$> readTVar bulbM
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ DeviceReplyType StateWifiFirmwareReply)
              (StateWifiFirmware fbwfBuild 0 fbwfVersion)

          pure $ (BSL.toChunks bytes)
      (Request (DeviceMessageType GetWifiInfoMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetWifiInfo header rest
        forM payloadE $ \payload -> do
          FakeBulbWifiInfo {..} <- atomically $ fbWifiInfo <$> readTVar bulbM
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ DeviceReplyType StateWifiInfoReply)
              (StateWifiInfo fbwiSignal fbwiTx fbwiRx 0)

          pure $ (BSL.toChunks bytes)
      (Request (LightMessageType GetLightMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetLight header rest
        forM payloadE $ \payload -> do
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ LightReplyType StateReply)
              (StateLight def 0 (LightPower 65535) (Label "FakeLight") 0)

          pure $ (BSL.toChunks bytes)
      (Request (DeviceMessageType GetVersionMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetVersion header rest
        forM payloadE $ \payload -> do
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ DeviceReplyType StateVersionReply)
              (StateVersion def Color1000 $ HardwareVersion 1233454)

          pure $ (BSL.toChunks bytes)
      (Request (DeviceMessageType GetLocationMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetLocation header rest
        forM payloadE $ \payload -> do
          t <- getCurrentLifxUTC
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ DeviceReplyType StateLocationReply)
              (StateGroup def (Label "Home") t)

          pure $ (BSL.toChunks bytes)
      (Request (DeviceMessageType GetGroupMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetGroup header rest
        forM payloadE $ \payload -> do
          t <- getCurrentLifxUTC
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ DeviceReplyType StateGroupReply)
              (StateGroup def (Label "Lab") t)

          pure $ (BSL.toChunks bytes)
      (Request (DeviceMessageType GetUnknown54Message)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetUnknown54 header rest
        forM payloadE $ \payload -> do
          t <- getCurrentLifxUTC
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ DeviceReplyType StateUnknown54Reply)
              (StateUnknown54 def (Label "") t)

          pure $ (BSL.toChunks bytes)
      _ -> (Left $ UnknownPacketError "") <$ (print $ "Header: " <> show header <> " Rest: " <> show rest)

  atomically $ modifyTVar'
    bulbM
    (\fb ->
      let
        fbw = fbWifiInfo fb
        fbwiRx' = fbwiRx fbw + (fromIntegral $ BSL.length bsl)
        fbw' = fbw { fbwiRx = fbwiRx' }
      in
        fb { fbWifiInfo = fbw' }
    )

  forM_ encoded $ \enc ->
    forM_ enc $ \msg -> do
      sendManyTo ss msg sa
      atomically $ modifyTVar'
        bulbM
        (\fb ->
          let
            fbw = fbWifiInfo fb
            fbwiTx' = fbwiTx fbw + (fromIntegral $ BSL.length bsl)
            fbw' = fbw { fbwiTx = fbwiTx' }
          in
            fb { fbWifiInfo = fbw' }
        )

data FakeBulb
  = FakeBulb
  { fbService      :: !Word8
  , fbPort         :: !Word32le
  , fbFirmware     :: !FakeBulbFirmware
  , fbWifiInfo     :: !FakeBulbWifiInfo
  , fbWifiFirmware :: !FakeBulbWifiFirmware
  , fbVersion      :: !FakeBulbVersion
  , fbState        :: !FakeBulbState
  , fbLocation     :: !FakeBulbLocation
  , fbStartTime    :: !LifxUTC
  }

instance Default FakeBulb where
  def
    = FakeBulb
    { fbService = 1
    , fbPort = 56700
    , fbFirmware = def
    , fbWifiInfo = def
    , fbWifiFirmware = def
    , fbVersion = def
    , fbState = def
    , fbLocation = def
    , fbStartTime = LifxUTC 0
    }

data FakeBulbFirmware
  = FakeBulbFirmware
  { fbfBuild :: Word64le
  , fbfVersion :: Word32le
  }

instance Default FakeBulbFirmware where
  def
    = FakeBulbFirmware
    { fbfBuild = 0
    , fbfVersion = 0
    }

data FakeBulbWifiInfo
  = FakeBulbWifiInfo
  { fbwiSignal :: Float32le
  , fbwiTx :: Word32le
  , fbwiRx :: Word32le
  }

instance Default FakeBulbWifiInfo where
  def
    = FakeBulbWifiInfo
    { fbwiSignal = 0
    , fbwiTx = 0
    , fbwiRx = 0
    }

data FakeBulbWifiFirmware
  = FakeBulbWifiFirmware
  { fbwfBuild :: Word64le
  , fbwfVersion :: Word32le
  }

instance Default FakeBulbWifiFirmware where
  def
    = FakeBulbWifiFirmware
    { fbwfBuild = 0
    , fbwfVersion = 0
    }

data FakeBulbVersion
  = FakeBulbVersion
  { fbvVendor :: VendorId
  , fbvProduct :: ProductId
  , fbvVersion :: Word32le
  }

instance Default FakeBulbVersion where
  def
    = FakeBulbVersion
    { fbvVendor = VendorId 1
    , fbvProduct = Color1000
    , fbvVersion = 0
    }

data FakeBulbLocation
  = FakeBulbLocation
  { fblLocationid :: LocationId
  , fblLabel :: Label "location"
  }

instance Default FakeBulbLocation where
  def
    = FakeBulbLocation
    { fblLocationid = def
    , fblLabel = def
    }

data FakeBulbState
  = FakeBulbState
  { fbsColor :: HSBK
  }

instance Default FakeBulbState where
  def
    = FakeBulbState
    { fbsColor = def
    }

fakeBulb
  :: LifxUTC
  -> FakeBulb
fakeBulb t
  = def
  { fbService = 1
  , fbPort = 56700
  , fbFirmware = def
      { fbfBuild = 12312435456465
      , fbfVersion = 1234354
      }
  , fbWifiInfo = def
      { fbwiSignal = 1.0e-5
      , fbwiTx = 0
      , fbwiRx = 0
      }
  , fbWifiFirmware = def
      { fbwfBuild = 123845464500000
      }
  , fbVersion = def
      { fbvVendor = VendorId 1
      , fbvProduct = Color1000
      }
  , fbState = def
      { fbsColor = def
          { hsbkKelvin = 3500
          }
      }
  , fbStartTime = t
  }

-- func configureBulb(port uint16, hasColor bool) {
-- 	bulb.service = controlifx.UdpService
-- 	bulb.port = port
-- 
-- 	// Mock HostFirmware.
-- 	bulb.hostFirmware.build = 1467178139000000000
-- 	bulb.hostFirmware.version = 1968197120
-- 
-- 	// Mock WifiInfo.
-- 	bulb.wifiInfo.signal = 1e-5
-- 
-- 	// Mock WifiFirmware.
-- 	bulb.wifiFirmware.build = 1456093684000000000
-- 
-- 	if hasColor {
-- 		bulb.version.vendor = controlifx.Color1000VendorId
-- 		bulb.version.product = controlifx.Color1000ProductId
-- 	} else {
-- 		bulb.version.vendor = controlifx.White800HighVVendorId
-- 		bulb.version.product = controlifx.White800HighVProductId
-- 	}
-- 
-- 	bulb.state.color.Kelvin = 3500
-- 
-- 	// Extra.
-- 	bulb.startTime = time.Now().UnixNano()
-- }

main :: IO ()
main = do
  ssSocket <- socket AF_INET Datagram defaultProtocol
  let
    port = 56700
    addr = SockAddrInet port 0

  nics <- getNetworkInterfaces
  let
    Just eth0 = L.find ((== "eth0") . NI.name) nics

  when (isSupportedSocketOption Broadcast)
    (setSocketOption ssSocket Broadcast 1)
  bind ssSocket addr
  t <- getCurrentLifxUTC
  fakeBulbM <- newTVarIO (fakeBulb t)
  asReceiveThread <- lightReceiveThread eth0 ssSocket fakeBulbM
  print "Thread launched"

  waitCatch asReceiveThread
  pure ()
