{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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

import            Home.Lights.LIFX.Types


-- | Documented and undocumented message types from controlifx
-- SetSiteType                         = 1
-- GetServiceType                      = 2
-- StateServiceType                    = 3
-- GetTimeType                         = 4
-- SetTimeType                         = 5
-- StateTimeType                       = 6
-- GetResetSwitchType                  = 7
-- StateResetSwitchType                = 8
-- GetDummyLoadType                    = 9
-- SetDummyLoadType                    = 10
-- StateDummyLoadType                  = 11
-- GetHostInfoType                     = 12
-- StateHostInfoType                   = 13
-- GetHostFirmwareType                 = 14
-- StateHostFirmwareType               = 15
-- GetWifiInfoType                     = 16
-- StateWifiInfoType                   = 17
-- GetWifiFirmwareType                 = 18
-- StateWifiFirmwareType               = 19
-- GetPowerType                        = 20
-- SetPowerType                        = 21
-- StatePowerType                      = 22
-- GetLabelType                        = 23
-- SetLabelType                        = 24
-- StateLabelType                      = 25
-- GetTagsType                         = 26
-- SetTagsType                         = 27
-- StateTagsType                       = 28
-- GetTagLabelsType                    = 29
-- SetTagLabelsType                    = 30
-- StateTagLabelsType                  = 31
-- GetVersionType                      = 32
-- StateVersionType                    = 33
-- GetInfoType                         = 34
-- StateInfoType                       = 35
-- GetMcuRailVoltageType               = 36
-- StateMcuRailVoltageType             = 37
-- SetRebootType                       = 38
-- SetFactoryTestModeType              = 39
-- DisableFactoryTestModeType          = 40
-- StateFactoryTestModeType            = 41
-- StateSiteType                       = 42
-- StateRebootType                     = 43
-- SetPanGatewayType                   = 44
-- AcknowledgementType                 = 45
-- SetFactoryResetType                 = 46
-- StateFactoryResetType               = 47
-- GetLocationType                     = 48
-- SetLocationType                     = 49
-- StateLocationType                   = 50
-- GetGroupType                        = 51
-- SetGroupType                        = 52
-- StateGroupType                      = 53
-- GetOwnerType                        = 54
-- SetOwnerType                        = 55
-- StateOwnerType                      = 56
-- GetFactoryTestModeType              = 57
-- EchoRequestType                     = 58
-- EchoResponseType                    = 59
-- LightGetType                        = 101
-- LightSetColorType                   = 102
-- LightSetWaveformType                = 103
-- LightSetDimAbsoluteType             = 104
-- LightSetDimRelativeType             = 105
-- LightSetRgbwType                    = 106
-- LightStateType                      = 107
-- LightGetRailVoltageType             = 108
-- LightStateRailVoltageType           = 109
-- LightGetTemperatureType             = 110
-- LightStateTemperatureType           = 111
-- LightSetCalibrationCoefficientsType = 112
-- LightSetSimpleEventType             = 113
-- LightGetSimpleEventType             = 114
-- LightStateSimpleEventType           = 115
-- LightGetPowerType                   = 116
-- LightSetPowerType                   = 117
-- LightStatePowerType                 = 118
-- LightSetWaveformOptionalType        = 119
-- WanGetType                          = 201
-- WanSetType                          = 202
-- WanStateType                        = 203
-- WanGetAuthKeyType                   = 204
-- WanSetAuthKeyType                   = 205
-- WanStateAuthKeyType                 = 206
-- WanSetKeepAliveType                 = 207
-- WanStateKeepAliveType               = 208
-- WanSetHostType                      = 209
-- WanGetHostType                      = 210
-- WanStateHostType                    = 211
-- WifiGetType                         = 301
-- WifiSetType                         = 302
-- WifiStateType                       = 303
-- WifiGetAccessPointsType             = 304
-- WifiSetAccessPointType              = 305
-- WifiStateAccessPointsType           = 306
-- WifiGetAccessPointType              = 307
-- WifiStateAccessPointType            = 308
-- WifiSetAccessPointBroadcastType     = 309
-- SensorGetAmbientLightType           = 401
-- SensorStateAmbientLightType         = 402
-- SensorGetDimmerVoltageType          = 403
-- SensorStateDimmerVoltageType        = 404

--data PH a = PH Word16le (Proxy a)
--data P a = P (PH a) a
--
--data PHolder a = PHolder (Proxy a, Word16le)
--
--instance KnownNat n => Binary (PHolder n) where
--  get = do
--    mip <- Bin.get
--    if mip == (fromIntegral $ natVal (Proxy :: Proxy n))
--    then
--      pure $ PHolder (Proxy :: Proxy n, mip)
--    else
--      fail "uh"
--
--  put (PHolder (Proxy, mip)) = Bin.put mip
--
--instance KnownNat n => Binary (PHolder n) where
--  get = do
--    mip <- Bin.get
--    if mip == (fromIntegral $ natVal (Proxy :: Proxy n))
--    then
--      pure $ PHolder (Proxy :: Proxy n, mip)
--    else
--      fail "uh"
--
--  put (PHolder (Proxy, mip)) = Bin.put mip
--  parseJSON (A.String s)
--    | s == pack (symbolVal (Proxy :: Proxy s))
--      = return (Proxy :: Proxy s)
--
--  parseJSON _ = mzero
--

maxMessagesPerSecond
  :: Word8
maxMessagesPerSecond
  = 20


listCached
  :: SharedState
  -> IO [Light]
listCached SharedState {..}
  = HM.elems <$> (atomically $ readTVar ssDevices)

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


-- | 54 undocumented messages
--  $\NUL\NUL\DC4\244Q\n\SO\208s\143\134\191\175\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL6\NUL\NUL\NUL
--



--type family MessageId2 st = p | p -> st where
--  MessageId2 StateLight = (GetLight, Void)
--  MessageId2 StateLightPower = (GetLightPower, SetLightPower)
--
--
--type family In n h where
--  In n (n, r) = True
--  In n (r, n) = True
--
--type family GetP h where
--  GetP (n, r) = n
--
--type family SetP h where
--  SetP (r, Void) = Void
--  SetP (r, n) = n
--
--
--data SrcP
--  = GetPacket
--  | SetPacket
--
--type family Ps srcp tpl where
--  Ps GetPacket (g,s) = g
--  Ps SetPacket (g,s) = s
--
--outerGetT
--  :: forall get res g
--   . 
--   ( res ~ MessageId2 get
--     , g ~ GetP res
--     )
--  => SharedState
--  -> Device
--  -> (SharedState -> Packet get -> SockAddr -> BSL.ByteString -> IO ())
--  -> IO ()
--outerGetT ss d cb
--  = do
--  p <- newPacket' ss cb
--  let
--    fp = p @g msgCons
--    np = fp { pFrameAddress = (pFrameAddress fp) { faTarget = deviceIdToTarget $ dDeviceId d} }
--  sendToDevice ss d np
--
--
--newPacketT' ss@(SharedState {..}) runCb
--  = newPacket ss runCb id
--
--newPacketT
--  :: forall get
--   .  ()
--  => SharedState
--  -> (SharedState -> Packet get -> SockAddr -> BSL.ByteString -> IO ())
--  -> (Packet a -> Packet a)
--  -> IO (a -> Packet a)
--newPacketT ss@(SharedState {..}) runCb modify
--  = do
--  nextSeq <- ssNextSeq
--  setCallbackForSeq ss nextSeq $ CallbackWrap decodePacket runCb
--  pure $ modify . mkPacket
--    SingleTagged
--    uniqueSource
--    (word64leToTarget 0)
--    NoAckRequired
--    NoResRequired
--    nextSeq
--    (msgTypP (Proxy :: Proxy a))














getCurrentLifxUTC
  :: IO LifxUTC
getCurrentLifxUTC
  = (LifxUTC . floor . (* 1000000000) . utcTimeToPOSIXSeconds) <$> getCurrentTime


-- 1 1 Original 1000 Yes No No
-- 1 3 Color 650 Yes No No
-- 1 10 White 800 (Low Voltage) No No No
-- 1 11 White 800 (High Voltage) No No No
-- 1 18 White 900 BR30 (Low Voltage) No No No
-- 1 20 Color 1000 BR30 Yes No No
-- 1 22 Color 1000 Yes No No
-- 1 27 LIFX A19 Yes No No
-- 1 28 LIFX BR30 Yes No No
-- 1 29 LIFX+ A19 Yes Yes No
-- 1 30 LIFX+ BR30 Yes Yes No
-- 1 31 LIFX Z Yes No Yes
-- 1 32 LIFX Z 2 Yes No Yes
-- 1 36 LIFX Downlight Yes No No
-- 1 37 LIFX Downlight Yes No No
-- 1 43 LIFX A19 Yes No No
-- 1 44 LIFX BR30 Yes No No
-- 1 45 LIFX+ A19 Yes Yes No
-- 1 46 LIFX+ BR30 Yes Yes No
-- 1 49 LIFX Mini Yes No No
-- 1 50 LIFX Mini Day and Dusk No No No
-- 1 51 LIFX Mini White No No No
-- 1 52 LIFX GU10 Yes No No

mkFrame
  :: WithSize a
  => Packet a
  -> Tagged
  -> UniqueSource
  -> Frame
mkFrame par tag
  = Frame (size par) 0 tag HasFrameAddress 1024

mkFrameAddress
  :: Target
  -> AckRequired
  -> ResRequired
  -> Sequence
  -> FrameAddress
mkFrameAddress tar
  = FrameAddress tar (UnusedMac $ Mac ((), (), (), (), (), ())) ()

mkProtocolHeader
  :: MessageType
  -> ProtocolHeader
mkProtocolHeader typ
  = ProtocolHeader 0 (Request typ) ()

mkPacket
  :: ( WithSize a
     , Binary a
     )
  => Tagged
  -> UniqueSource
  -> Target
  -> AckRequired
  -> ResRequired
  -> Sequence
  -> MessageType
  -> a
  -> Packet a
mkPacket tag src tar ack res sequ typ pay
  =
  let
    f = mkFrame p tag src
    fa = mkFrameAddress tar ack res sequ
    ph = mkProtocolHeader typ
    -- Only make a refernce to `p` here to "tie the knot" since mkFrame needs the `p` size
    p = Packet f fa ph pay
  in
    p

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


broadcast
  :: ( Binary a
     , Show a
     )
  => Socket
  -> SockAddr
  -> a
  -> IO ()
broadcast sock bcast a
  =
  let
    enc = Bin.encode a
  in
    sendManyTo sock (BSL.toChunks enc) bcast



deviceIdToTarget
  :: DeviceId
  -> Target
deviceIdToTarget (DeviceId m)
  = Target m


decodeHeader
  :: Maybe UniqueSource
  -> BSL.ByteString
  -> Except HeaderDecodeError (Header, BSL.ByteString)
decodeHeader uniqSrc bs
  = case Bin.decodeOrFail bs of
  Left (str, offset, err) ->
    throwE $ NotAHeader err bs str offset
  Right (rema, cons, hdr) -> do
    let
      packetSize = fSize $ hFrame hdr
      packetSource = fSource $ hFrame hdr
    when (packetSize /= fromIntegral (BSL.length bs))
      $ throwE $ ImproperSizeInHeader hdr bs rema cons
    when (isJust uniqSrc && Just packetSource /= uniqSrc)
      $ throwE $ ImproperSourceInHeader hdr bs rema cons
    pure (hdr, rema)

decodePacket
  :: ( Binary a
     , WithSize a
     )
  => Header
  -> BSL.ByteString
  -> Except PayloadDecodeError (Packet a)
decodePacket hdr rema
  = case Bin.decodeOrFail rema of
  Left (str, offset, err) ->
    throwE $ PayloadDecodeFailed hdr rema str offset err
  Right (_, _, payload) ->
    pure $ packetFromHeader hdr payload

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
onStateService ss@(SharedState {..}) Packet {..} sa _orig
  = do
  forM_ (socketAddrToDeviceSocketAddr sa) $ \sa' -> do
    let
      incomingDevice = Device sa' (DeviceId $ unTarget $ faTarget pFrameAddress)
    moreInfo <- atomically $ do
      devs <- readTVar ssDevices
      case dDeviceId incomingDevice `HM.lookup` devs of
        Just l ->
          if (lDevice l /= incomingDevice && False)
          then do
            --writeTVar ssDevices $ HM.insert (dDeviceId incomingDevice) (Light incomingDevice Nothing Nothing Nothing Nothing Nothing) devs
            pure $ map (flip uncurry (ss, incomingDevice)) queries
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
          pure $ map (flip uncurry (ss, incomingDevice)) queries

    sequence_ moreInfo
  where
    StateService {..} = pPayload
    queries = [getLocation, getGroup, getLabel, getLightPower, getLight, getVersion, getHostFirmware, getWifiFirmware]

sendToDevice
  :: ( Binary a
     )
  => SharedState
  -> Device
  -> Packet a
  -> IO ()
sendToDevice SharedState {..} Device {..} packet
  = sendManyTo ssSocket (BSL.toChunks bytes) (SockAddrInet p w)
  where
    DeviceSocketAddress p (DeviceAddress (unWord32le -> w)) = dAddr
    bytes = Bin.encode np
    np = packet { pFrameAddress = (pFrameAddress packet) { faTarget = deviceIdToTarget dDeviceId} }

updateWifiFirmware
  :: SharedState
  -> Device
  -> Packet StateWifiFirmware
  -> IO ()
updateWifiFirmware SharedState {..} Device {..} Packet {..}
  = do
  atomically $ do
    devs <- readTVar ssDevices
    let
      newDevs = HM.adjust
        (\l@(Light {..}) ->
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
  = do
  outerGet ss d GetWifiFirmware $ \_ p@(Packet {..}) _ _ -> do
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
  = do
  atomically $ do
    devs <- readTVar ssDevices
    let
      newDevs = HM.adjust
        (\l@(Light {..}) ->
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
  = do
  outerGet ss d GetHostFirmware $ \_ p@(Packet {..}) _ _ -> do
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
  = do
  atomically $ do
    devs <- readTVar ssDevices
    let
      newDevs = HM.adjust
        (\l@(Light {..}) ->
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
  = do
  outerGet ss d GetVersion $ \_ p@(Packet {..}) _ _ -> do
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
  = do
  atomically $ do
    devs <- readTVar ssDevices
    let
      newDevs = HM.adjust (\l@(Light {..}) -> l { lLocation = Just stlLabel } ) dDeviceId devs
    writeTVar ssDevices newDevs
  where
    StateLocation {..} = pPayload


getLocation
  :: SharedState
  -> Device
  -> IO ()
getLocation ss d
  = do
  outerGet ss d GetLocation $ \_ p@(Packet {..}) _ _ -> do
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
  = do
  atomically $ do
    devs <- readTVar ssDevices
    let
      newDevs = HM.adjust (\l@(Light {..}) -> l { lGroup = Just stgLabel } ) dDeviceId devs
    writeTVar ssDevices newDevs
  where
    StateGroup {..} = pPayload

getGroup
  :: SharedState
  -> Device
  -> IO ()
getGroup ss d
  = outerGet ss d GetGroup $ \_ p@(Packet {..}) _ _ -> do
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
  = do
  atomically $ do
    devs <- readTVar ssDevices
    let
      newDevs = HM.adjust (\l@(Light {..}) -> l { lLabel = Just stlaLabel } ) dDeviceId devs
    writeTVar ssDevices newDevs
  where
    StateLabel {..} = pPayload

getLabel
  :: SharedState
  -> Device
  -> IO ()
getLabel ss d
  = outerGet ss d GetLabel $ \_ p@(Packet {..}) _ _ -> do
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
  = do
  atomically $ do
    devs <- readTVar ssDevices
    let
      newDevs = HM.adjust (\l@(Light {..}) -> l { lPower = Just stlpLevel } ) dDeviceId devs
    writeTVar ssDevices newDevs
  where
    StateLightPower {..} = pPayload

getLightPower
  :: SharedState
  -> Device
  -> IO ()
getLightPower ss d
  = outerGet ss d GetLightPower $ \_ p@(Packet {..}) _ _ -> do
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
  = do
  atomically $ do
    devs <- readTVar ssDevices
    let
      newDevs = HM.adjust
         (\l@(Light {..})
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
  = outerGet ss d GetLight $ \_ p@(Packet {..}) _ _ -> do
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


newDiscoveryPacket
  :: SharedState
  -> (SharedState -> Packet StateService -> SockAddr -> BSL.ByteString -> IO ())
  -> IO (Packet GetService)
newDiscoveryPacket ss@(SharedState {..}) runCb
  = do
  pp <- newPacket ss runCb
    $ \p@(Packet {..}) ->
      let
        f = pFrame
        pFrame' = f { fTagged = AllTagged }
      in
        p { pFrame = pFrame' }
  pure $ pp GetService

newPacket'
  :: forall a
   . ( MessageIdC a )
  => SharedState
  -> (SharedState -> Packet (StateReply a) -> SockAddr -> BSL.ByteString -> IO ())
  -> IO (a -> Packet a)
newPacket' ss@(SharedState {..}) runCb
  = newPacket ss runCb id

newPacket
  :: forall a
   . ( MessageIdC a )
  => SharedState
  -> (SharedState -> Packet (StateReply a) -> SockAddr -> BSL.ByteString -> IO ())
  -> (Packet a -> Packet a)
  -> IO (a -> Packet a)
newPacket ss@(SharedState {..}) runCb modify
  = do
  nextSeq <- ssNextSeq
  setCallbackForSeq ss nextSeq $ CallbackWrap decodePacket runCb
  pure $ modify . mkPacket
    SingleTagged
    uniqueSource
    (word64leToTarget 0)
    NoAckRequired
    NoResRequired
    nextSeq
    (msgTypP (Proxy :: Proxy a))


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

setCallbackForSeq
  :: ( MonadIO m )
  => SharedState
  -> Sequence
  -> CallbackWrap
  -> m ()
setCallbackForSeq SharedState {..} sequ cont
  = liftIO
  $ atomically
  $ writeArray ssReplyCallbacks (unSequence sequ) cont

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
  ssLogLevel <- fromJust . (maybe (Just LogError) toLogLevel) <$> lookupEnv "LOG_LEVEL"

  let
    ssLogFunction onLevel msg
      = do
      when (onLevel >= ssLogLevel) $
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


packetFromHeader
  :: ( Binary a
     , WithSize a
     )
  => Header
  -> a
  -> Packet a
packetFromHeader Header {..} payload
  = Packet hFrame hFrameAddress hProtocolHeader payload

mkTestFrame
  :: WithSize a
  => Packet a
  -> Tagged
  -> UniqueSource
  -> Frame
mkTestFrame par tag
  = Frame (size par) 0 tag HasFrameAddress 1024

mkTestFrameAddress
  :: Target
  -> AckRequired
  -> ResRequired
  -> Sequence
  -> FrameAddress
mkTestFrameAddress tar
  = FrameAddress tar (UnusedMac $ Mac ((), (), (), (), (), ())) ()

mkTestProtocolHeader
  :: Direction
  -> ProtocolHeader
mkTestProtocolHeader typ
  = ProtocolHeader 0 typ ()

mkTestPacket
  :: ( WithSize a
     , Binary a
     )
  => Tagged
  -> UniqueSource
  -> Target
  -> AckRequired
  -> ResRequired
  -> Sequence
  -> Direction
  -> a
  -> Packet a
mkTestPacket tag src tar ack res sequ typ pay
  =
  let
    f = mkTestFrame p tag src
    fa = mkTestFrameAddress tar ack res sequ
    ph = mkTestProtocolHeader typ
    p = Packet f fa ph pay
  in
    p


sendToLight
  :: ( Binary a
     )
  => SharedState
  -> Light
  -> Packet a
  -> IO ()
sendToLight ss@(SharedState {}) Light {..} packet
  = sendToDevice ss d packet
  where
    d@(Device {..}) = lDevice

