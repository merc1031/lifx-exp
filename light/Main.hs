{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import            Control.Arrow               ( first
                                              , (&&&)
                                              )
import            Control.Monad               ( void )
import            Control.Monad.Except
import            Data.Binary                 ( Binary )
import            Data.Function
import            Data.Generics.Product
import            Data.Generics.Sum
import            GHC.Generics
import            Lens.Micro.Platform
import            Control.Concurrent
import            Control.Concurrent.Async
import            Control.Concurrent.STM
import            Control.Exception
import            Control.Lens.Reified
import            Control.Monad
import            Control.Monad.Trans.Reader
import            Data.Default
import            Data.Proxy
import            Data.Semigroup              ( (<>) )
import            Data.String                 ( IsString (..) )
import            Data.Word
import            Network.Info                ( getNetworkInterfaces )
import            Network.Socket              ( Socket (..)
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
import            Options.Applicative
import            Text.Pretty.Simple
import qualified  Data.Binary                 as  Bin
import qualified  Data.ByteString             as  BS
import qualified  Data.ByteString.Lazy        as  BSL
import qualified  Data.List                   as  L
import qualified  Data.Time.Clock.POSIX       as  POSIX
import qualified  Network.Info                as  NI

import            Lib
import            Home.Lights.LIFX.Transport
import            Home.Lights.LIFX.Types


type OnLifxState a
    = ( TVar FakeBulb
      , (Direction -> StateReply a -> Packet (StateReply a))
      , Header
      , BSL.ByteString
      )


class MessageId a => OnLifx a where
  onReq
    :: Proxy a
    -> ReaderT (OnLifxState a) IO (Either MorphedError ShouldRespond)


data SomePayload
  = forall a. (OnLifx a, MessageId a) => SomePayload (Proxy a)


directionToSomePayload
  :: Direction
  -> Either String SomePayload
directionToSomePayload
  = \case
  Request (DeviceMessageType x) -> deviceMessagetoSomePayload x
  Request (LightMessageType x) -> lightMessagetoSomePayload x
  m -> Left $ "No reply for packet type" <> show m
  where
    deviceMessagetoSomePayload
      = \case
      GetServiceMessage -> Right $ SomePayload $ Proxy @GetService
      GetHostFirmwareMessage -> Right $ SomePayload $ Proxy @GetHostFirmware
      GetWifiFirmwareMessage -> Right $ SomePayload $ Proxy @GetWifiFirmware
      GetWifiInfoMessage -> Right $ SomePayload $ Proxy @GetWifiInfo
      GetVersionMessage -> Right $ SomePayload $ Proxy @GetVersion
      GetLocationMessage -> Right $ SomePayload $ Proxy @GetLocation
      GetGroupMessage -> Right $ SomePayload $ Proxy @GetGroup
      SetLabelMessage -> Right $ SomePayload $ Proxy @SetLabel
      GetUnknown54Message -> Right $ SomePayload $ Proxy @GetUnknown54
      m -> Left $ "No reply for packet type" <> show m
    lightMessagetoSomePayload
      = \case
      GetLightMessage -> Right $ SomePayload $ Proxy @GetLight
      SetColorMessage -> Right $ SomePayload $ Proxy @SetColor
      SetLightPowerMessage -> Right $ SomePayload $ Proxy @SetLightPower
      SetInfraredMessage -> Right $ SomePayload $ Proxy @SetInfrared
      SetWaveformMessage -> Right $ SomePayload $ Proxy @SetWaveform
      SetWaveformOptionalMessage -> Right $ SomePayload $ Proxy @SetWaveformOptional
      m -> Left $ "No reply for packet type" <> show m


instance OnLifx GetService where
  onReq _
    = do
    (_bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @GetService decodedHeader rest

    forM payloadE $ \_payload -> do

      let
        packet
          = pp
          (Reply $ DeviceReplyType StateServiceReply)
          (StateService 1 56700)

      pure $ YesResponse (stream packet)


instance OnLifx GetHostFirmware where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @GetHostFirmware decodedHeader rest

    forM payloadE $ \_payload -> do
      FakeBulbFirmware {..}
        <- liftIO $ atomically
            $ fbFirmware
          <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ DeviceReplyType StateHostFirmwareReply)
          (StateHostFirmware fbfBuild 0 fbfVersion)

      pure $ YesResponse (stream packet)


instance OnLifx GetWifiFirmware where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @GetWifiFirmware decodedHeader rest

    forM payloadE $ \_payload -> do
      FakeBulbWifiFirmware {..}
        <- liftIO $ atomically
            $ fbWifiFirmware
          <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ DeviceReplyType StateWifiFirmwareReply)
          (StateWifiFirmware fbwfBuild 0 fbwfVersion)

      pure $ YesResponse (stream packet)


instance OnLifx GetWifiInfo where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @GetWifiInfo decodedHeader rest

    forM payloadE $ \_payload -> do
      FakeBulbWifiInfo {..}
        <- liftIO $ atomically
            $ fbWifiInfo
          <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ DeviceReplyType StateWifiInfoReply)
          (StateWifiInfo fbwiSignal fbwiTx fbwiRx 0)

      pure $ YesResponse (stream packet)


instance OnLifx GetLight where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @GetLight decodedHeader rest

    forM payloadE $ \_payload -> do
      (FakeBulbState {..}, label)
        <- liftIO $ atomically
            $ (fbState &&& (^. typed @(Label "name")))
          <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ LightReplyType StateLightReply)
          (StateLight fbsColor 0 fbsLightPowerLevel label 0)

      pure $ YesResponse (stream packet)


instance OnLifx GetVersion where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @GetVersion decodedHeader rest

    forM payloadE $ \_payload -> do
      FakeBulbVersion {..}
        <- liftIO $ atomically
            $ fbVersion
          <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ DeviceReplyType StateVersionReply)
          (StateVersion def fbvProduct $ HardwareVersion 0)

      pure $ YesResponse (stream packet)


instance OnLifx GetLocation where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @GetLocation decodedHeader rest

    forM payloadE $ \_payload -> do
      t <- liftIO $ getCurrentLifxUTC
      FakeBulbLocation {..}
        <- liftIO $ atomically
            $ fbLocation
          <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ DeviceReplyType StateLocationReply)
          (StateLocation fblLocationId fblLabel t)

      pure $ YesResponse (stream packet)


instance OnLifx GetGroup where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @GetGroup decodedHeader rest

    forM payloadE $ \_payload -> do
      t <- liftIO $ getCurrentLifxUTC
      FakeBulbGroup {..}
        <- liftIO $ atomically
            $ fbGroup
          <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ DeviceReplyType StateGroupReply)
          (StateGroup fbgGroupId fbgLabel t)

      pure $ YesResponse (stream packet)


instance OnLifx SetLabel where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @SetLabel decodedHeader rest

    forM payloadE $ \Packet { pPayload } -> do

      label
        <- liftIO $ atomically $ do
          modifyTVar'
            bulbM
            (& typed @(Label "name") .~ (selLabel pPayload))
          fbLabel <$> readTVar bulbM


      let
        packet
          = pp
          (Reply $ DeviceReplyType StateLabelReply)
          (StateLabel label)

      pure $ shouldRespond decodedHeader (stream packet)


instance OnLifx SetColor where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @SetColor decodedHeader rest

    forM payloadE $ \p@(Packet { pPayload = SetColor {..} }) -> do
      liftIO $ printBanner "Set Color Payload " (pPayload p)

      -- Should really spion off a thread to handle the duration aspect of chaning color...
      (FakeBulbState {..}, label)
        <- liftIO $ atomically $ do
          modifyTVar'
            bulbM
            (& typed @FakeBulbState . typed @HSBK .~ secColor)

          (fbState &&& (^. typed @(Label "name")))
            <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ LightReplyType StateLightReply)
          (StateLight fbsColor 0 fbsLightPowerLevel label 0)

      pure $ shouldRespond decodedHeader (stream packet)


instance OnLifx SetLightPower where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @SetLightPower decodedHeader rest

    forM payloadE $ \Packet { pPayload = SetLightPower {..} } -> do

      -- If there is a previous power thread kill it (TODO maybe use a channel and a persistent thread)
      (powerThreadV, powerThread)
        <- liftIO $ atomically $ do
          pVar <- fbPowerThread <$> readTVar bulbM
          (pVar, ) <$> readTVar pVar

      forM_ powerThread $ liftIO . cancel

      LightPower lightPowerLevel
        <- liftIO $ atomically
            $ fbsLightPowerLevel
            . fbState
          <$> readTVar bulbM

      start <- liftIO $ POSIX.getPOSIXTime

      pThread <- liftIO $ async $ do

        let
          end
            = floor start + selpDuration
          loop
            = do
            threadDelay
              $ floor @Double
              $ 2 * ((1 * 1000000) / fromIntegral maxMessagesPerSecond) -- Only use 1/2 of our message allotment by sleeping 2 times as long
            now <- POSIX.getPOSIXTime

            let
              percentage
                = (now - start) / (fromIntegral end - start)
              smear
                = lightPowerLevel + ((unLightPower selpLevel - lightPowerLevel) * floor percentage)

            atomically
              $ modifyTVar'
                  bulbM
                  (& typed @FakeBulbState . typed @LightPower .~ LightPower smear)
            -- start = 10     end = 40      now = 20
            --         20           80
            when (floor now < end)
              loop
        -- | Start the loop
        loop

      liftIO $ atomically
        $ modifyTVar' powerThreadV
        (const $ Just pThread)

      let
        packet
          = pp
          (Reply $ LightReplyType StateLightPowerReply)
          (StateLightPower $ LightPower lightPowerLevel)

      pure $ shouldRespond decodedHeader (stream packet)


instance OnLifx SetInfrared where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @SetInfrared decodedHeader rest

    forM payloadE $ \Packet { pPayload } -> do
      infraredBrightness
        <- liftIO $ atomically $ do
          modifyTVar'
            bulbM
            (& typed @FakeBulbState . typed @Word16le .~ seiBrightness pPayload)
          fbsInfraredBrightness . fbState <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ LightReplyType StateInfraredReply)
          (StateInfrared infraredBrightness)

      pure $ shouldRespond decodedHeader (stream packet)


instance OnLifx SetWaveform where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @SetWaveform decodedHeader rest

    forM payloadE $ \p@(Packet { pPayload = SetWaveform {..} }) -> do
      liftIO $ printBanner "Set Waveform Payload " (pPayload p)

      (FakeBulbState {..}, label)
        <- liftIO $ atomically
            $ (fbState &&& (^. typed @(Label "name")))
          <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ LightReplyType StateLightReply)
          (StateLight fbsColor 0 fbsLightPowerLevel label 0)

      pure $ YesResponse (stream packet)


instance OnLifx SetWaveformOptional where
  onReq _
    = do
    (bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @SetWaveformOptional decodedHeader rest

    forM payloadE $ \p@(Packet { pPayload = SetWaveformOptional {..} }) -> do
      liftIO $ printBanner "Set Waveform Optional Payload " (pPayload p)

      (FakeBulbState {..}, label)
        <- liftIO $ atomically
--              $ (fbState &&& (^. typed @(Label "name")))  -- | Simple arrow
--                $ ((. field @"fbState") &&& (^. typed @(Label "name")))  -- | Arrowed full lens
            $ (^. (runGetter $ (,) <$> Getter (field @"fbState") <*>  Getter (typed @(Label "name"))))  -- | General lens composition
          <$> readTVar bulbM

      let
        packet
          = pp
          (Reply $ LightReplyType StateLightReply)
          (StateLight fbsColor 0 fbsLightPowerLevel label 0)

      pure $ YesResponse (stream packet)


instance OnLifx GetUnknown54 where
  onReq _
    = do
    (_bulbM, pp, decodedHeader, rest) <- ask
    let
      payloadE
        = runExcept
        $ withExcept MorphedPayloadError
        $ decodePacket @GetUnknown54 decodedHeader rest

    forM payloadE $ \_payload -> do
      t <- liftIO $ getCurrentLifxUTC

      let
        _packet
          = pp
          (Reply $ DeviceReplyType StateUnknown54Reply)
          (StateUnknown54 def (Label "") t)

      pure $ NoResponse


stream
  :: Binary a
  => Packet a
  -> [BS.ByteString]
stream
  = BSL.toChunks
  . Bin.encode


printBanner
  :: Show a
  => String
  -> a
  -> IO ()
printBanner msg p
  = do
  print @String $ replicate100 '*'
  print $ msg <> show p
  print @String $ replicate100 '*'


lightReceiveThread
  :: NI.NetworkInterface
  -> Socket
  -> TVar FakeBulb
  -> IO (Async ())
lightReceiveThread nic ss bulbM
  = async $ forever $ do
  print @String "About to recv"

  replicateM_ 4 $ print @String spaces100
  (!bsl, sa) <- first BSL.fromStrict <$> recvFrom ss 1500

  let
    headerE
      = runExcept
      $ decodeHeader Nothing bsl

  incrRx bsl

  encoded <- forM headerE $ \(decodedHeader, rest) -> do

    let
      sequ
        = faSequence
        $ hFrameAddress decodedHeader
      ackR
        = faAckRequired
        $ hFrameAddress decodedHeader
      resR
        = faResRequired
        $ hFrameAddress decodedHeader
      uniqS
        = fSource
        $ hFrame decodedHeader
      msgT
        = phType
        $ hProtocolHeader decodedHeader
      nicToTarget
        = case NI.mac nic of
        NI.MAC _b1 _b2 _b3 _b4 _b5 _b6 ->
          Target
            $ Mac
            $ (0xd0, 0x74, 0x8f, 0x86, 0xbf, 0xaf)
      pp
        :: ( Binary a
           , WithSize a
           )
        => Direction
        -> a
        -> Packet a
      pp
        = preamble
        uniqS
        nicToTarget
        sequ

    -- | Acknowledge when required
    when (ackR == AckRequired) $ do

      let
        packet
          = pp
          (Reply $ DeviceReplyType AcknowledgementReply)
          (Acknowledgement)
        msg
          = stream packet

      sendManyTo ss msg sa
      incrTx msg

    print
      $ "Message " <> show msgT <> " "
      <> show ackR <> " "
      <> show resR

    case directionToSomePayload msgT of
      Right (SomePayload p) ->
        flip runReaderT (bulbM, pp, decodedHeader, rest) $ onReq p
      Left err ->
          (Left $ UnknownPacketError "")
       <$ (liftIO $ print $ "Header: " <> show decodedHeader <> " Rest: " <> show rest <> " Message: " <> err)


  -- | Unwrap Maybe
  forM_ encoded $ \enc ->
    -- | Unwrape Morphed Error
    forM_ enc $ \case
      NoResponse -> pure ()
      YesResponse msg -> do
        sendManyTo ss msg sa
        incrTx msg

  print =<< (atomically $ readTVar bulbM)

  replicateM_ 4 $ print @String spaces100

  where
    incrTx (BS.concat -> BS.length -> fromIntegral -> len)
      = atomically
      $ modifyTVar'
          bulbM
          (& typed @FakeBulbWifiInfo . field @"fbwiTx" %~ (+ len))

    incrRx (BSL.length -> fromIntegral -> len)
      = atomically
      $ modifyTVar'
          bulbM
          (& typed @FakeBulbWifiInfo . field @"fbwiRx" %~ (+ len))

    preamble uniqS nicToTarget sequ
      = mkPacket
      SingleTagged
      uniqS
      nicToTarget
      NoAckRequired
      NoResRequired
      sequ



data MorphedError
  = MorphedPayloadError !PayloadDecodeError
  | UnknownPacketError !String
  deriving Show


data ShouldRespond
  = YesResponse ![BS.ByteString]
  | NoResponse
  deriving Show


shouldRespond
  :: Header
  -> [BS.ByteString]
  -> ShouldRespond
shouldRespond (hFrameAddress -> faResRequired -> ResRequired) bsl
  = YesResponse bsl
shouldRespond _ _
  = NoResponse


spaces100
  :: IsString a
  => a
spaces100
  = replicate100 ' '


replicate100
  :: IsString a
  => Char
  -> a
replicate100 c
  = fromString $ replicate 100 c

{-

variadicFunction :: VariadicReturnClass r => RequiredArgs -> r
variadicFunction reqArgs = variadicImpl reqArgs mempty

class VariadicReturnClass r where
   variadicImpl :: RequiredArgs -> AccumulatingType -> r

instance VariadicReturnClass ActualReturnType where
   variadicImpl reqArgs acc = constructActualResult reqArgs acc

instance (ArgClass a, VariadicReturnClass r) => VariadicReturnClass (a -> r) where
   variadicImpl reqArgs acc = \a -> variadicImpl reqArgs (specialize a `mappend` acc)

-}

--getFields :: ManyGetter r => r
--getFields = getters
--
--class ManyGetter r where
--   getters :: AccumulatingType -> r
--
--instance VariadicReturnClass ActualReturnType where
--   variadicImpl reqArgs acc = constructActualResult reqArgs acc
--
--instance (ArgClass a, VariadicReturnClass r) => VariadicReturnClass (a -> r) where
--   variadicImpl reqArgs acc = \a -> variadicImpl reqArgs (specialize a `mappend` acc)

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
  , fbLabel        :: !(Label "name")
  , fbPowerThread  :: (TVar (Maybe (Async ())))
  }
  deriving Generic


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
    <> show fbLabel <> " "
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
    , fbState = def
    , fbLocation = def
    , fbGroup = def
    , fbStartTime = LifxUTC 0
    , fbLabel = Label "Fake Light"
    , fbPowerThread = undefined
    }


data FakeBulbFirmware
  = FakeBulbFirmware
  { fbfBuild   :: !Word64le
  , fbfVersion :: !Word32le
  }
  deriving (Show, Generic)


instance Default FakeBulbFirmware where
  def
    = FakeBulbFirmware
    -- | Got from dump from real lights
    { fbfBuild = 1511412934000000000
    , fbfVersion = 131144
    }


data FakeBulbWifiInfo
  = FakeBulbWifiInfo
  { fbwiSignal :: !Float32le
  , fbwiTx     :: !Word32le
  , fbwiRx     :: !Word32le
  }
  deriving (Show, Generic)


instance Default FakeBulbWifiInfo where
  def
    = FakeBulbWifiInfo
    { fbwiSignal = 0
    , fbwiTx = 0
    , fbwiRx = 0
    }


data FakeBulbWifiFirmware
  = FakeBulbWifiFirmware
  { fbwfBuild   :: !Word64le
  , fbwfVersion :: !Word32le
  }
  deriving (Show, Generic)


instance Default FakeBulbWifiFirmware where
  def
    = FakeBulbWifiFirmware
    { fbwfBuild = 0
    , fbwfVersion = 0
    }


data FakeBulbVersion
  = FakeBulbVersion
  { fbvVendor  :: !VendorId
  , fbvProduct :: !ProductId
  , fbvVersion :: !Word32le
  }
  deriving (Show, Generic)


instance Default FakeBulbVersion where
  def
    = FakeBulbVersion
    { fbvVendor = VendorId 1
    , fbvProduct = LIFXPBR30
    , fbvVersion = 0
    }


data FakeBulbLocation
  = FakeBulbLocation
  { fblLocationId :: !LocationId
  , fblLabel      :: !(Label "location")
  }
  deriving (Show, Generic)


instance Default FakeBulbLocation where
  def
    = FakeBulbLocation
    -- | Got from dump from real lights
    { fblLocationId = LocationId $ ByteId16 [191,85,176,70,10,79,235,23,234,91,5,109,79,82,97,227]
    , fblLabel = Label "Home"
    }


data FakeBulbGroup
  = FakeBulbGroup
  { fbgGroupId :: !GroupId
  , fbgLabel   :: !(Label "group")
  }
  deriving (Show, Generic)


instance Default FakeBulbGroup where
  def
    = FakeBulbGroup
    -- | Got from dump from real lights
    { fbgGroupId = GroupId $ ByteId16 [47,223,129,99,224,228,225,11,76,216,163,23,127,156,239,247]
    , fbgLabel = Label "Lab"
    }


data FakeBulbState
  = FakeBulbState
  { fbsColor              :: !HSBK
  , fbsLightPowerLevel    :: !LightPower
  , fbsInfraredBrightness :: !Word16le
  }
  deriving (Show, Generic)


instance Default FakeBulbState where
  def
    = FakeBulbState
    { fbsColor = def
    , fbsLightPowerLevel = LightPower 0
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


main
  :: IO ()
main
  = do
  ssSocket <- socket AF_INET Datagram defaultProtocol

  let
    port
      = 56700
    addr
      = SockAddrInet port 0

  nics <- getNetworkInterfaces
  let
    Just eth0
      = L.find ((== "eth0") . NI.name) nics

  when (isSupportedSocketOption Broadcast)
    (setSocketOption ssSocket Broadcast 1)

  bind ssSocket addr

  t <- getCurrentLifxUTC

  powerThread <- newTVarIO Nothing
  fakeBulbM <- newTVarIO (fakeBulb t powerThread)

  asReceiveThread <- lightReceiveThread eth0 ssSocket fakeBulbM

  print @String "Thread launched"

  void $ waitCatch asReceiveThread
  pure ()
