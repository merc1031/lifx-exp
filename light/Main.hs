{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
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
import Control.Exception
import Data.Default
import Data.Word
import qualified Data.Time.Clock.POSIX as POSIX
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data MorphedError
  = MorphedPayloadError PayloadDecodeError
  | UnknownPacketError String
  deriving Show

data ShouldRespond
  = YesResponse [BS.ByteString]
  | NoResponse
  deriving Show

shouldRespond
  :: Header
  -> [BS.ByteString]
  -> ShouldRespond
shouldRespond (hFrameAddress -> faResRequired -> ResRequired) bsl = YesResponse bsl
shouldRespond _ _ = NoResponse

lightReceiveThread
  :: NI.NetworkInterface
  -> Socket
  -> TVar FakeBulb
  -> IO (Async ())
lightReceiveThread nic ss bulbM
  = async $ forever $ do
  print "About to recv"
  print "                                                                                                    "
  print "                                                                                                    "
  print "                                                                                                    "
  print "                                                                                                    "
  (!bs, sa) <- recvFrom ss 1500
  let
    bsl = BSL.fromStrict bs
    headerE = runExcept $ decodeHeader Nothing bsl

  incrRx bsl

  encoded <- forM headerE $ \(header, rest) -> do
    let
      sequ = faSequence $ hFrameAddress header
      ackR = faAckRequired $ hFrameAddress header
      resR = faResRequired $ hFrameAddress header
      uniqS = fSource $ hFrame header
      msgT = phType $ hProtocolHeader header
      nicToTarget = case NI.mac nic of
        NI.MAC b1 b2 b3 b4 b5 b6 -> Target $ Mac $ (0xd0, 0x74, 0x8f, 0x86, 0xbf, 0xaf)

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
        msg = BSL.toChunks bytes

      sendManyTo ss msg sa
      incrTx msg

    print $ "Message " <> show msgT <> " " <> show ackR <> " " <> show resR
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

          pure $ YesResponse (BSL.toChunks bytes)
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

          pure $ YesResponse (BSL.toChunks bytes)
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

          pure $ YesResponse (BSL.toChunks bytes)
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

          pure $ YesResponse (BSL.toChunks bytes)
      (Request (LightMessageType GetLightMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetLight header rest
        forM payloadE $ \payload -> do
          FakeBulbState {..} <- atomically $ fbState <$> readTVar bulbM
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ LightReplyType StateLightReply)
              (StateLight fbsColor 0 fbsLightPowerLevel fbsLabel 0)

          print packet
          pure $ YesResponse (BSL.toChunks bytes)
      (Request (DeviceMessageType GetVersionMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetVersion header rest
        forM payloadE $ \payload -> do
          FakeBulbVersion {..} <- atomically $ fbVersion <$> readTVar bulbM
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
              (StateVersion def fbvProduct $ HardwareVersion 0)

          pure $ YesResponse (BSL.toChunks bytes)
      (Request (DeviceMessageType GetLocationMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetLocation header rest
        forM payloadE $ \payload -> do
          t <- getCurrentLifxUTC
          FakeBulbLocation {..} <- atomically $ fbLocation <$> readTVar bulbM
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
              (StateLocation fblLocationId fblLabel t)

          pure $ YesResponse (BSL.toChunks bytes)
      (Request (DeviceMessageType GetGroupMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @GetGroup header rest
        forM payloadE $ \payload -> do
          t <- getCurrentLifxUTC
          FakeBulbGroup {..} <- atomically $ fbGroup <$> readTVar bulbM
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
              (StateGroup fbgGroupId fbgLabel t)

          pure $ YesResponse (BSL.toChunks bytes)
      (Request (DeviceMessageType SetLabelMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @SetLabel header rest
        forM payloadE $ \Packet { pPayload } -> do
          t <- getCurrentLifxUTC
          label <- atomically $ do
            modifyTVar' bulbM (\b -> b { fbState = (fbState b) { fbsLabel = selLabel pPayload }})
            fbsLabel . fbState <$> readTVar bulbM


          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ DeviceReplyType StateLabelReply)
              (StateLabel label)

          pure $ shouldRespond header (BSL.toChunks bytes)
      (Request (LightMessageType SetColorMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @SetColor header rest
        forM payloadE $ \p@(Packet { pPayload = SetColor {..} }) -> do
          print $ "Set Color Payload " <> show (pPayload p)
          -- Should really spion off a thread to handle the duration aspect of chaning color...
          FakeBulbState {..} <- atomically $ do
            modifyTVar' bulbM (\b -> b { fbState = (fbState b) { fbsColor = secColor }})
            fbState <$> readTVar bulbM
          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ LightReplyType StateLightReply)
              (StateLight fbsColor 0 fbsLightPowerLevel fbsLabel 0)

          print packet
          pure $ shouldRespond header (BSL.toChunks bytes)
--          pure $ YesResponse (BSL.toChunks bytes)
      (Request (LightMessageType SetLightPowerMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @SetLightPower header rest
        forM payloadE $ \Packet { pPayload = SetLightPower {..} } -> do

          -- If there is a previous power thread kill it (TODO maybe use a channel and a persistent thread)
          powerThreadV <- atomically $ fbPowerThread <$> readTVar bulbM
          powerThread <- atomically $ readTVar powerThreadV
          case powerThread of
            Just p -> cancel p
            Nothing -> pure ()

          LightPower lightPowerLevel <- atomically $ fbsLightPowerLevel . fbState <$> readTVar bulbM
          start <- POSIX.getPOSIXTime

          async $ do
            let
              end = floor start + selpDuration
              loop = do
                threadDelay $ floor $ 2 * ((1 * 1000000) / fromIntegral maxMessagesPerSecond) -- Only use 1/2 of our message allotment by sleeping 2 times as long
                now <- POSIX.getPOSIXTime
                let
                  percentage = (now - start) / (fromIntegral end - start)
                  smear = lightPowerLevel + ((unLightPower selpLevel - lightPowerLevel) * floor percentage)
                atomically $ modifyTVar' bulbM (\b -> b { fbState = (fbState b) { fbsLightPowerLevel = LightPower smear }})
                -- start = 10     end = 40      now = 20
                --         20           80
                when (floor now < end)
                  loop
            loop

          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ LightReplyType StateLightPowerReply)
              (StateLightPower $ LightPower lightPowerLevel)

          print packet
          pure $ shouldRespond header (BSL.toChunks bytes)
      (Request (LightMessageType SetInfraredMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @SetInfrared header rest
        forM payloadE $ \Packet { pPayload } -> do
          infraredBrightness <- atomically $ do
            modifyTVar' bulbM (\b -> b { fbState = (fbState b) { fbsInfraredBrightness = seiBrightness pPayload }})
            fbsInfraredBrightness . fbState <$> readTVar bulbM

          let
            bytes = Bin.encode packet
            packet = mkTestPacket
              SingleTagged
              uniqS
              nicToTarget
              NoAckRequired
              NoResRequired
              sequ
              (Reply $ LightReplyType StateInfraredReply)
              (StateInfrared infraredBrightness)

          pure $ shouldRespond header (BSL.toChunks bytes)
      (Request (LightMessageType SetWaveformOptionalMessage)) -> do
        let
          payloadE = runExcept $ withExcept MorphedPayloadError $ decodePacket @SetWaveformOptional header rest
        forM payloadE $ \payload -> do
          FakeBulbState {..} <- atomically $ fbState <$> readTVar bulbM
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
              (StateLight fbsColor 0 fbsLightPowerLevel fbsLabel 0)

          print packet
          pure $ YesResponse (BSL.toChunks bytes)
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

          pure $ NoResponse
      _ -> (Left $ UnknownPacketError "") <$ (print $ "Header: " <> show header <> " Rest: " <> show rest)



  -- | Unwrap Maybe
  forM_ encoded $ \enc ->
    -- | Unwrape Morphed Error
    forM_ enc $ \case
      NoResponse -> pure ()
      YesResponse msg -> do
        sendManyTo ss msg sa
        incrTx msg
  print =<< (atomically $ readTVar bulbM)
  print "                                                                                                    "
  print "                                                                                                    "
  print "                                                                                                    "
  print "                                                                                                    "
  print "                                                                                                    "
  where
    incrTx bss = atomically $ modifyTVar'
      bulbM
      (\fb ->
        let
          fbw = fbWifiInfo fb
          fbwiTx' = fbwiTx fbw + (fromIntegral $ BS.length $ BS.concat bss)
          fbw' = fbw { fbwiTx = fbwiTx' }
        in
          fb { fbWifiInfo = fbw' }
      )
    incrRx bsl = atomically $ modifyTVar'
      bulbM
      (\fb ->
        let
          fbw = fbWifiInfo fb
          fbwiRx' = fbwiRx fbw + (fromIntegral $ BSL.length bsl)
          fbw' = fbw { fbwiRx = fbwiRx' }
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
  , fbGroup        :: !FakeBulbGroup
  , fbStartTime    :: !LifxUTC
  , fbPowerThread  :: (TVar (Maybe (Async ())))
  }

instance Show FakeBulb where
  show FakeBulb {..}
    = "FakeBulb "
    <> show fbService <> " "
    <> show fbPort <> " "
    <> show fbFirmware <> " "
    <> show fbWifiInfo <> " "
    <> show fbWifiFirmware <> " "
    <> show fbVersion <> " "
    <> show fbState <> " "
    <> show fbLocation <> " "
    <> show fbGroup <> " "
    <> show fbStartTime <> " "
    <> "PowerThread"

instance Default FakeBulb where
  def
    = FakeBulb
    { fbService = 1
    , fbPort = 56700
    , fbFirmware = def
    , fbWifiInfo = def
    , fbWifiFirmware = def
    , fbVersion = def
    , fbState = def { fbsLabel = Label "Fake Light" }
    , fbLocation = def { fblLabel = Label "Home" }
    , fbGroup = def { fbgLabel = Label "Lab" }
    , fbStartTime = LifxUTC 0
    , fbPowerThread = undefined
    }

data FakeBulbFirmware
  = FakeBulbFirmware
  { fbfBuild :: Word64le
  , fbfVersion :: Word32le
  }
  deriving Show


instance Default FakeBulbFirmware where
  def
    = FakeBulbFirmware
    { fbfBuild = 1511412934000000000
    , fbfVersion = 131144
    }

data FakeBulbWifiInfo
  = FakeBulbWifiInfo
  { fbwiSignal :: Float32le
  , fbwiTx :: Word32le
  , fbwiRx :: Word32le
  }
  deriving Show

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
  deriving Show

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
  deriving Show

instance Default FakeBulbVersion where
  def
    = FakeBulbVersion
    { fbvVendor = VendorId 1
    , fbvProduct = LIFXPBR30
    , fbvVersion = 0
    }

data FakeBulbLocation
  = FakeBulbLocation
  { fblLocationId :: LocationId
  , fblLabel :: Label "location"
  }
  deriving Show

instance Default FakeBulbLocation where
  def
    = FakeBulbLocation
    { fblLocationId = def
    , fblLabel = def
    }

data FakeBulbGroup
  = FakeBulbGroup
  { fbgGroupId :: GroupId
  , fbgLabel :: Label "group"
  }
  deriving Show

instance Default FakeBulbGroup where
  def
    = FakeBulbGroup
    { fbgGroupId = def
    , fbgLabel = def
    }

data FakeBulbState
  = FakeBulbState
  { fbsColor :: HSBK
  , fbsLightPowerLevel :: LightPower
  , fbsLabel :: Label "name"
  , fbsInfraredBrightness :: Word16le
  }
  deriving Show

instance Default FakeBulbState where
  def
    = FakeBulbState
    { fbsColor = def
    , fbsLightPowerLevel = LightPower 0
    , fbsLabel = Label ""
    , fbsInfraredBrightness = 0
    }

fakeBulb
  :: LifxUTC
  -> TVar (Maybe (Async ()))
  -> FakeBulb
fakeBulb t powerThread
  = def
  { fbService = 1
  , fbPort = 56700
  , fbFirmware = def
  , fbWifiInfo = def
      { fbwiSignal = 1.0e-5
      , fbwiTx = 0
      , fbwiRx = 0
      }
  , fbWifiFirmware = def
  , fbVersion = def
      { fbvVendor = VendorId 1
      , fbvProduct = LIFXPBR30
      }
  , fbState = def
      { fbsColor = def
          { hsbkKelvin = 3500
          }
      }
  , fbStartTime = t
  , fbPowerThread = powerThread
  }

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
  powerThread <- newTVarIO Nothing
  fakeBulbM <- newTVarIO (fakeBulb t powerThread)
  asReceiveThread <- lightReceiveThread eth0 ssSocket fakeBulbM
  print "Thread launched"

  waitCatch asReceiveThread
  pure ()
