{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Lib where

import            Control.Applicative           ( (<|>) )
import            Control.Arrow
import            Control.Concurrent
import            Control.Concurrent.Async
import            Control.Concurrent.STM
import            Control.Concurrent.STM.TQueue
import            Control.DeepSeq
import            Control.DeepSeq.Generics
import            Data.Void
import            GHC.Generics
import            Control.Exception
import            Control.Monad                 ( forM_
                                                , replicateM_
                                                , when
                                                , forever
                                                , void
                                                )
import            Control.Monad.Except
import            Control.Monad.IO.Class        ( MonadIO
                                                , liftIO
                                                )
import            Control.Monad.Trans.Except
import            Control.Monad.Trans.Control
import            Control.Monad.Trans.Maybe
import            Data.Array.MArray             ( writeArray
                                                , readArray
                                                , newArray_
                                                , newListArray
                                                )
import            Data.Binary                   ( Binary (..)
                                                , Get
                                                , Put
                                                )
import            Data.Binary.IEEE754
import            Data.Bits                     ( zeroBits
                                                , Bits(..)
                                                , FiniteBits(..)
                                                , bit
                                                , shiftR
                                                , shiftL
                                                , testBit
                                                )
import            Data.Bool                     ( bool )
import            Data.Char                     ( intToDigit
                                                , isPrint
                                                , toLower
                                                )
import            Data.Coerce
import            Data.Default
import            Data.Functor.Identity         ( Identity )
import            Data.Generics.Product
import            Data.Generics.Sum
import            Data.Hashable                 ( Hashable )
import            Data.Int                      ( Int8
                                                , Int16
                                                , Int32
                                                , Int64
                                                )
import            Data.Maybe                    ( isJust
                                                , fromJust
                                                )
import            Data.Monoid                   ( (<>) )
import            Data.Proxy
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
import            Data.Word                     ( Word8
                                                , Word16
                                                , Word32
                                                , Word64
                                                )
import            GHC.Prim
import            GHC.TypeLits
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
import            Network.Socket.ByteString
import            Numeric                       ( showHex )
import            Text.Printf
import            System.Environment
--import            Test.QuickCheck               (Arbitrary (..) )
import qualified  Data.Binary                   as Bin
import qualified  Data.Binary.Bits              as Bits
import qualified  Data.Binary.Get               as BinG
import qualified  Data.Binary.Put               as BinP
import qualified  Data.ByteString.Base16.Lazy   as BSL16
import qualified  Data.ByteString.Lazy          as BSL
import qualified  Data.HashMap.Strict           as HM
import qualified  Data.Text.Lazy                as TL
import qualified  Data.Text.Lazy.Encoding       as TLE
import qualified  Network.Info                  as NI

import            Home.Lights.LIFX.Transport
import            Home.Lights.LIFX.Types



maxMessagesPerSecond
  :: Word8
maxMessagesPerSecond
  = 20


listCached
  :: SharedState
  -> IO [Light]
listCached SharedState {..}
  = HM.elems <$> atomically (readTVar ssDevices)

hue
  :: Float
  -> Maybe Hue
hue v
  | v >= 0 && v <= 360 = Just $ Hue $ floor $ v / 360 * 65535
  | otherwise = Nothing

saturation
  :: Float
  -> Maybe Saturation
saturation v
  | v >= 0 && v <= 100 = Just $ Saturation $ floor $ v / 100 * 65535
  | otherwise = Nothing

brightness
  :: Float
  -> Maybe Brightness
brightness v
  | v >= 0 && v <= 100 = Just $ Brightness $ floor $ v / 100 * 65535
  | otherwise = Nothing

kelvin
  :: Float
  -> Maybe Kelvin
kelvin v
  | v >= 2500 && v <= 9000 = Just $ Kelvin $ floor v
  | otherwise = Nothing


getCurrentLifxUTC
  :: IO LifxUTC
getCurrentLifxUTC
  = (LifxUTC . floor . (* 1000000000) . utcTimeToPOSIXSeconds) <$> getCurrentTime


serviceUDP
  :: Word8
serviceUDP
  = 1






-- size
-- origin
-- tagged
-- addressable
-- protocol
-- source
-- target
-- reserved
-- reserved
-- ack
-- res
-- sequence
-- reserved
-- type
-- reserved
-- 16|2|1|1|12|32|64|48|6|1|1|8|64|16|16
-- ui|ui|b|b|ui|ui|ui|ui|r|b|b|ui|ui|ui|r
--                   [6]




uniqueSource
  :: UniqueSource
uniqueSource
  = UniqueSource 1234


receiveThread
  :: SharedState
  -> IO (Async ())
receiveThread ss@SharedState {..}
  = async $ forever $ do
  (!bs, sa) <- recvFrom ssSocket 1500
  let
    bsl = BSL.fromStrict bs
    headerE = runExcept $ decodeHeader (Just ssUniqueSource) bsl

  forM_ headerE $ \(header, rest) -> async $ do
    let
      Sequence sequ = faSequence $ hFrameAddress header
    cb <- atomically $ readArray ssReplyCallbacks sequ
    case cb of
      CallbackWrap {..} -> do
        let
          payloadE = runExcept $ runDecode header rest
        forM_ payloadE $ \payload ->
          runCallback ss payload sa bsl

onStateService
  :: SharedState
  -> Packet StateService
  -> SockAddr
  -> BSL.ByteString
  -> IO ()
onStateService ss@SharedState {..} Packet {..} sa _orig
  = forM_ (socketAddrToDeviceSocketAddr sa) $ \sa' -> do
  let
    incomingDevice = Device sa' (DeviceId $ unTarget $ faTarget pFrameAddress)
  moreInfo <- atomically $ do
    devs <- readTVar ssDevices
    case dDeviceId incomingDevice `HM.lookup` devs of
      Just l ->
        if lDevice l /= incomingDevice && False
        then
          --writeTVar ssDevices $ HM.insert (dDeviceId incomingDevice) (Light incomingDevice Nothing Nothing Nothing Nothing Nothing) devs
          pure $ map (`uncurry` (ss, incomingDevice)) queries
        else pure []
      Nothing -> do
        writeTVar ssDevices
          $ HM.insert
              (dDeviceId incomingDevice)
              (Light
                incomingDevice
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
              )
              devs
        pure $ map (`uncurry` (ss, incomingDevice)) queries

  sequence_ moreInfo
  where
    StateService {..} = pPayload
    queries = [getLocation, getGroup, getLabel, getLightPower, getLight, getVersion, getHostFirmware, getWifiFirmware]


updateWifiFirmware
  :: SharedState
  -> Device
  -> Packet StateWifiFirmware
  -> IO ()
updateWifiFirmware SharedState {..} Device {..} Packet {..}
  = atomically $ do
  devs <- readTVar ssDevices
  let
    newDevs = HM.adjust
      (\l@Light {..} ->
        l { lWifiFirmwareBuild = Just stwfBuild
          , lWifiFirmwareVersion = Just stwfVersion
          }
      )
      dDeviceId
      devs
  writeTVar ssDevices newDevs
  where
    StateWifiFirmware {..} = pPayload


getWifiFirmware
  :: SharedState
  -> Device
  -> IO ()
getWifiFirmware ss d
  = outerGet ss d GetWifiFirmware $ \_ p@Packet {..} _ _ -> do
  let
    StateWifiFirmware {..} = pPayload
  ssLogFunction ss LogDebug $ "Got wifi firmware: " <> show p
  updateWifiFirmware ss d p

updateHostFirmware
  :: SharedState
  -> Device
  -> Packet StateHostFirmware
  -> IO ()
updateHostFirmware SharedState {..} Device {..} Packet {..}
  = atomically $ do
  devs <- readTVar ssDevices
  let
    newDevs = HM.adjust
      (\l@Light {..} ->
        l { lHostFirmwareBuild = Just sthfBuild
          , lHostFirmwareVersion = Just sthfVersion
          }
      )
      dDeviceId
      devs
  writeTVar ssDevices newDevs
  where
    StateHostFirmware {..} = pPayload


getHostFirmware
  :: SharedState
  -> Device
  -> IO ()
getHostFirmware ss d
  = outerGet ss d GetHostFirmware $ \_ p@Packet {..} _ _ -> do
  let
    StateHostFirmware {..} = pPayload
  ssLogFunction ss LogDebug $ "Got host firmware: " <> show p
  updateHostFirmware ss d p


updateVersion
  :: SharedState
  -> Device
  -> Packet StateVersion
  -> IO ()
updateVersion SharedState {..} Device {..} Packet {..}
  = atomically $ do
  devs <- readTVar ssDevices
  let
    newDevs = HM.adjust
      (\l@Light {..} ->
        l { lProduct = Just stvProduct
          , lHardwareVersion = Just stvVersion
          }
      )
      dDeviceId
      devs
  writeTVar ssDevices newDevs
  where
    StateVersion {..} = pPayload


getVersion
  :: SharedState
  -> Device
  -> IO ()
getVersion ss d
  = outerGet ss d GetVersion $ \_ p@Packet {..} _ _ -> do
  let
    StateVersion {..} = pPayload
  ssLogFunction ss LogDebug $ "Got version: " <> show p
  updateVersion ss d p

updateLocation
  :: SharedState
  -> Device
  -> Packet StateLocation
  -> IO ()
updateLocation SharedState {..} Device {..} Packet {..}
  = atomically $ do
  devs <- readTVar ssDevices
  let
    newDevs = HM.adjust (\l@Light {..} -> l { lLocation = Just stlLabel } ) dDeviceId devs
  writeTVar ssDevices newDevs
  where
    StateLocation {..} = pPayload


getLocation
  :: SharedState
  -> Device
  -> IO ()
getLocation ss d
  = outerGet ss d GetLocation $ \_ p@Packet {..} _ _ -> do
  let
    StateLocation {..} = pPayload
  ssLogFunction ss LogDebug $ "Got location: " <> show p
  updateLocation ss d p


updateGroup
  :: SharedState
  -> Device
  -> Packet StateGroup
  -> IO ()
updateGroup SharedState {..} Device {..} Packet {..}
  = atomically $ do
  devs <- readTVar ssDevices
  let
    newDevs = HM.adjust (\l@Light {..} -> l { lGroup = Just stgLabel } ) dDeviceId devs
  writeTVar ssDevices newDevs
  where
    StateGroup {..} = pPayload

getGroup
  :: SharedState
  -> Device
  -> IO ()
getGroup ss d
  = outerGet ss d GetGroup $ \_ p@Packet {..} _ _ -> do
  let
    StateGroup {..} = pPayload
  ssLogFunction ss LogDebug $ "Got Group: " <> show p
  updateGroup ss d p

updateLabel
  :: SharedState
  -> Device
  -> Packet StateLabel
  -> IO ()
updateLabel SharedState {..} Device {..} Packet {..}
  = atomically $ do
  devs <- readTVar ssDevices
  let
    newDevs = HM.adjust (\l@Light {..} -> l { lLabel = Just stlaLabel } ) dDeviceId devs
  writeTVar ssDevices newDevs
  where
    StateLabel {..} = pPayload

getLabel
  :: SharedState
  -> Device
  -> IO ()
getLabel ss d
  = outerGet ss d GetLabel $ \_ p@Packet {..} _ _ -> do
  let
    StateLabel {..} = pPayload
  ssLogFunction ss LogDebug $ "Got Label: " <> show p
  updateLabel ss d p

updateLightPower
  :: SharedState
  -> Device
  -> Packet StateLightPower
  -> IO ()
updateLightPower SharedState {..} Device {..} Packet {..}
  = atomically $ do
  devs <- readTVar ssDevices
  let
    newDevs = HM.adjust (\l@Light {..} -> l { lPower = Just stlpLevel } ) dDeviceId devs
  writeTVar ssDevices newDevs
  where
    StateLightPower {..} = pPayload

getLightPower
  :: SharedState
  -> Device
  -> IO ()
getLightPower ss d
  = outerGet ss d GetLightPower $ \_ p@Packet {..} _ _ -> do
  let
    StateLightPower {..} = pPayload
  ssLogFunction ss LogDebug $ "Got LightPower: " <> show p
  updateLightPower ss d p

updateLight
  :: SharedState
  -> Device
  -> Packet StateLight
  -> IO ()
updateLight SharedState {..} Device {..} Packet {..}
  = atomically $ do
  devs <- readTVar ssDevices
  let
    newDevs = HM.adjust
       (\l@Light {..}
           -> l
           { lPower = Just stliPower
           , lColor = Just stliColor
           , lLabel = Just stliLabel
           }
       )
       dDeviceId
       devs
  writeTVar ssDevices newDevs
  where
    StateLight {..} = pPayload

getLight
  :: SharedState
  -> Device
  -> IO ()
getLight ss d
  = outerGet ss d GetLight $ \_ p@Packet {..} _ _ -> do
  let
    StateLight {..} = pPayload
  ssLogFunction ss LogDebug $ "Got Light: " <> show p
  updateLight ss d p

outerGet
  :: forall get
   . MessageIdC get
  => SharedState
  -> Device
  -> get
  -> (SharedState -> Packet (StateReply get) -> SockAddr -> BSL.ByteString -> IO ())
  -> IO ()
outerGet ss d pay cb
  = do
  p <- newPacket' ss cb
  let
    fp = p pay
    np = fp { pFrameAddress = (pFrameAddress fp) { faTarget = deviceIdToTarget $ dDeviceId d} }
  sendToDevice ss d np



socketAddrToDeviceSocketAddr
  :: SockAddr
  -> Maybe DeviceSocketAddress
socketAddrToDeviceSocketAddr (SockAddrInet pa ha)
  = Just $ DeviceSocketAddress pa (DeviceAddress $ Word32le ha)
socketAddrToDeviceSocketAddr _
  = Nothing

discoveryThread
  :: SharedState
  -> SockAddr
  -> IO (Async ())
discoveryThread ss@SharedState {..} bcast
  = async $ forever $ do
  gsp <- newDiscoveryPacket ss onStateService
  broadcast
    ssSocket
    bcast
    gsp
  threadDelay $ 10 * 1000000

toLogLevel
  :: String
  -> Maybe LogLevel
toLogLevel (map toLower -> s)
  = go s
  where
    go = \case
      "error" -> Just LogError
      "info" -> Just LogInfo
      "debug" -> Just LogDebug
      _ -> Nothing

mkState
  :: IO AppState
mkState
  = do
  ssLogLevel <- fromJust . maybe (Just LogError) toLogLevel <$> lookupEnv "LOG_LEVEL"

  let
    ssLogFunction onLevel msg
      = when (onLevel >= ssLogLevel) $
      putStrLn msg

  nSeq <- newTVarIO (Sequence 0)
  ssNextSeq <- pure $ atomically $ do
    val@(Sequence inner) <- readTVar nSeq
    writeTVar nSeq $! Sequence (inner + 1)
    pure val

  ssSocket <- socket AF_INET Datagram defaultProtocol
  let
    bcast = SockAddrInet (fromIntegral port) (tupleToHostAddress (255,255,255,255))
    port = 56700
    addr = SockAddrInet (port + 1) 0 -- If we bind to 56700 we receive our own GetService broadcast...

  when (isSupportedSocketOption Broadcast)
    (setSocketOption ssSocket Broadcast 1)
  bind ssSocket addr
  ssReplyCallbacks <- atomically $ newArray_ (0, 255)
  ssDevices <- newTVarIO mempty

  let
    ssUniqueSource = uniqueSource
    sharedState = SharedState {..}
  asReceiveThread <- receiveThread sharedState
  asDiscoveryThread <- discoveryThread sharedState bcast

  pure $ AppState sharedState asReceiveThread asDiscoveryThread





