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

module Home.Lights.LIFX.Types where

import            Control.Applicative           ( (<|>) )
import            Control.Concurrent.Async
import            Control.Concurrent.STM
import            Control.DeepSeq
import            Control.DeepSeq.Generics
import            GHC.Generics
import            Control.Exception
import            Control.Monad                 ( replicateM_
                                                , when
                                                , void
                                                )
import            Control.Monad.Except
import            Data.Binary                   ( Binary (..)
                                                , Put
                                                )
import            Data.Binary.IEEE754
import            Data.Bits                     ( Bits(..)
                                                , bit
                                                , shiftR
                                                , shiftL
                                                , testBit
                                                )
import            Data.Bool                     ( bool )
import            Data.Char                     ( isPrint )
import            Data.Default
import            Data.Hashable                 ( Hashable )
import            Data.Int                      ( Int16
                                                , Int32
                                                , Int64
                                                )
import            Data.Monoid                   ( (<>) )
import            Data.Proxy
import            Data.Word                     ( Word8
                                                , Word16
                                                , Word32
                                                , Word64
                                                )
import            GHC.TypeLits
import            Network.Socket                ( Socket (..)
                                                , SockAddr (..)
                                                , hostAddressToTuple
                                                , PortNumber
                                                )
import            Text.Printf
import qualified  Data.Binary                   as Bin
import qualified  Data.Binary.Get               as BinG
import qualified  Data.Binary.Put               as BinP
import qualified  Data.ByteString.Lazy          as BSL
import qualified  Data.HashMap.Strict           as HM
import qualified  Data.Text.Lazy                as TL
import qualified  Data.Text.Lazy.Encoding       as TLE

class MessageId a where
  type StateReply a

  msgId :: a -> Word16le
  msgId _ = msgIdP (Proxy :: Proxy a)

  msgIdP :: Proxy a -> Word16le
  msgIdP _ = msgId @a undefined


  msgTyp :: a -> MessageType
  msgTyp _ = msgTypP (Proxy :: Proxy a)

  msgTypP :: Proxy a -> MessageType
  msgTypP _ = msgTyp @a undefined



newtype Word16le
  = Word16le { unWord16le :: Word16 }
  deriving newtype (Num, Real, Enum, Integral, Show, Read, Eq, Ord, Bits, NFData)

newtype Word32le
  = Word32le { unWord32le :: Word32 }
  deriving newtype (Num, Real, Enum, Integral, Show, Read, Eq, Ord, Bits, NFData)

newtype Word64le
  = Word64le { unWord64le :: Word64 }
  deriving newtype (Num, Real, Enum, Integral, Show, Read, Eq, Ord, Bits, NFData)


instance Binary Word16le where
  put
    = BinP.putWord16le . unWord16le
  get
    = Word16le <$> BinG.getWord16le

instance Binary Word32le where
  put
    = BinP.putWord32le . unWord32le
  get
    = Word32le <$> BinG.getWord32le

instance Binary Word64le where
  put
    = BinP.putWord64le . unWord64le
  get
    = Word64le <$> BinG.getWord64le


newtype Int16le
  = Int16le { unInt16le :: Int16 }
  deriving newtype (Num, Real, Enum, Integral, Show, Read, Eq, Ord, Bits, NFData)

newtype Int32le
  = Int32le { unInt32le :: Int32 }
  deriving newtype (Num, Real, Enum, Integral, Show, Read, Eq, Ord, Bits, NFData)

newtype Int64le
  = Int64le { unInt64le :: Int64 }
  deriving newtype (Num, Real, Enum, Integral, Show, Read, Eq, Ord, Bits, NFData)

instance Binary Int16le where
  put
    = BinP.putInt16le . unInt16le
  get
    = Int16le <$> BinG.getInt16le

instance Binary Int32le where
  put
    = BinP.putInt32le . unInt32le
  get
    = Int32le <$> BinG.getInt32le

instance Binary Int64le where
  put
    = BinP.putInt64le . unInt64le
  get
    = Int64le <$> BinG.getInt64le

newtype Float32le
  = Float32le { unFloat32le :: Float }
  deriving newtype (Num, Real, Fractional, RealFrac, RealFloat, Floating, Show, Read, Eq, Ord, NFData)

instance Binary Float32le where
    put (Float32le f) = putFloat32le f
    get = Float32le <$> getFloat32le

data Tagged
  = SingleTagged -- Encodes as 0, means FrameAddress `target` must be a MAC
  | AllTagged -- Encodes as 1, means FrameAddress must be 0 and will be sent to all lights
  deriving (Show, Eq, Generic)

instance NFData Tagged where
  rnf
    = genericRnfV1


data Addressable
  = NoFrameAddress
  | HasFrameAddress -- Encodes as 1, always, meaning a `target` field exists
  deriving (Show, Eq, Generic)

instance NFData Addressable where
  rnf
    = genericRnfV1

data AckRequired
  = NoAckRequired
  | AckRequired
  deriving (Show, Eq, Generic)

instance NFData AckRequired where
  rnf
    = genericRnfV1

data ResRequired
  = NoResRequired
  | ResRequired
  deriving (Show, Eq, Generic)

instance NFData ResRequired where
  rnf
    = genericRnfV1


data Reserved (d :: Symbol) a
  = Reserved

data Sized (n :: Nat) a


data DeviceIdentifier
  = IdMac
  | IdIp
  | IdName
  deriving (Show, Eq)

-- Layout in bits:   16|2|1|1|12|32
--                   ui|ui|b|b|ui|ui
data Frame
  = Frame
  { fSize        :: !Word16le
  , fOrigin      :: !Word8 -- 0
  , fTagged      :: !Tagged
  , fAddressable :: !Addressable -- 1
  , fProtocol    :: !Word16le -- 1024
  , fSource      :: !UniqueSource
  }
  deriving (Show, Eq, Generic)

instance Default Frame where
  def
    = Frame
    { fSize = 0
    , fOrigin = 0
    , fTagged = AllTagged
    , fAddressable = HasFrameAddress
    , fProtocol = 1024
    , fSource = UniqueSource 0
    }

instance NFData Frame where
  rnf
    = genericRnfV1


-- Layout in bits:   64|48|6|1|1|8
--                   ui|ui|r|b|b|ui
--                      [6]
data FrameAddress
  = FrameAddress
  { faTarget      :: !Target
  , faReserved    :: !UnusedMac -- 0
  , faReserved2   :: !()
  , faAckRequired :: !AckRequired
  , faResRequired :: !ResRequired
  , faSequence    :: !Sequence
  }
  deriving (Show, Eq, Generic)

instance Default FrameAddress where
  def
    = FrameAddress
    { faTarget = word64leToTarget 0
    , faReserved = UnusedMac $ Mac ((), (), (), (), (), ())
    , faReserved2 = ()
    , faAckRequired = NoAckRequired
    , faResRequired = NoResRequired
    , faSequence = Sequence 0
    }

instance NFData FrameAddress where
  rnf
    = genericRnfV1


newtype UnusedMac
  = UnusedMac (Mac ())
  deriving (Show, Eq, Generic)

instance NFData UnusedMac where
  rnf
    = genericRnfV1

-- Layout in bits:   64|16|16
--                   ui|ui|r
data ProtocolHeader
  = ProtocolHeader
  { phReserved  :: !Word64le
  , phType      :: !Direction
  , phReserved2 :: !()
  }
  deriving (Show, Eq, Generic)

instance Default ProtocolHeader where
  def
    = ProtocolHeader
    { phReserved = 0
    , phType = Request $ DeviceMessageType GetServiceMessage
    , phReserved2 = ()
    }

instance NFData ProtocolHeader where
  rnf
    = genericRnfV1


data Packet a
  = Packet
  { pFrame          :: Frame -- ^ Frame must be lazy or we cannot tie the knot to fill in the true packet size
  , pFrameAddress   :: !FrameAddress
  , pProtocolHeader :: !ProtocolHeader
  , pPayload        :: !a
  }
  deriving (Show, Eq)

instance Binary a => Binary (Packet a) where
  put Packet {..}
    = do
    Bin.put pFrame
    Bin.put pFrameAddress
    Bin.put pProtocolHeader
    Bin.put pPayload

  get
    = do
    pFrame <- Bin.get
    pFrameAddress <- Bin.get
    pProtocolHeader <- Bin.get
    pPayload <- Bin.get
    pure Packet {..}


newtype Hue
  = Hue Word16le
  deriving (Show, Eq, Num, Ord, Enum, Real, Integral, Binary)

newtype Saturation
  = Saturation Word16le
  deriving (Show, Eq, Num, Ord, Enum, Real, Integral, Binary)

newtype Brightness
  = Brightness Word16le
  deriving (Show, Eq, Num, Ord, Enum, Real, Integral, Binary)

newtype Kelvin
  = Kelvin Word16le
  deriving (Show, Eq, Num, Ord, Enum, Real, Integral, Binary)

-- Layout in bits:   16|16|16|16
--                   ui|ui|ui|ui
data HSBK
  = HSBK
  { hsbkHue        :: !Hue -- 0-65535
  , hsbkSaturation :: !Saturation -- 0-65535
  , hsbkBrightness :: !Brightness -- 0-65535
  , hsbkKelvin     :: !Kelvin --2500-9000
  }
  deriving (Show, Eq)

instance Default HSBK where
  def
    = HSBK
    { hsbkHue = 0
    , hsbkSaturation = 0
    , hsbkBrightness = 65535
    , hsbkKelvin = 2700
    }


-- | 32 bytes
newtype Label (l :: Symbol)
  = Label { unLabel :: TL.Text }
  deriving (Show, Eq)

instance Binary (Label n) where
  put
    = Bin.put . unLabel
  get
    = (mkLabel . TLE.decodeUtf8) <$> BinG.getLazyByteString 32

data Direction
  = Request !MessageType
  | Reply !ReplyType
  deriving (Show, Eq, Generic)

instance NFData Direction where
  rnf
    = genericRnfV1


data MessageType
  = DeviceMessageType !DeviceMessage
  | LightMessageType !LightMessage
  | MultiZoneMessageType !MultiZoneMessage
  deriving (Show, Eq, Generic)

instance NFData MessageType where
  rnf
    = genericRnfV1


data ReplyType
  = DeviceReplyType !DeviceReply
  | LightReplyType !LightReply
  | MultiZoneReplyType !MultiZoneReply
  deriving (Show, Eq, Generic)

instance NFData ReplyType where
  rnf
    = genericRnfV1


data DeviceMessage
  = GetServiceMessage
  | GetHostInfoMessage
  | GetHostFirmwareMessage
  | GetWifiInfoMessage
  | GetWifiFirmwareMessage
  | GetPowerMessage
  | SetPowerMessage
  | GetLabelMessage
  | SetLabelMessage
  | GetVersionMessage
  | GetInfoMessage
  | GetLocationMessage
  | SetLocationMessage
  | GetGroupMessage
  | SetGroupMessage
  | GetUnknown54Message
  | EchoMessage
  deriving (Show, Eq, Generic)

instance NFData DeviceMessage where
  rnf
    = genericRnfV1

data LightMessage
  = GetLightMessage
  | SetColorMessage
  | SetWaveformMessage
  | SetWaveformOptionalMessage
  | GetLightPowerMessage
  | SetLightPowerMessage
  | GetInfraredMessage
  | SetInfraredMessage
  deriving (Show, Eq, Generic)

instance NFData LightMessage where
  rnf
    = genericRnfV1


data MultiZoneMessage
  = SetColorZonesMessage
  | GetColorZonesMessage
  deriving (Show, Eq, Generic)

instance NFData MultiZoneMessage where
  rnf
    = genericRnfV1

data DeviceReply
  = StateServiceReply
  | StateHostInfoReply
  | StateHostFirmwareReply
  | StateWifiInfoReply
  | StateWifiFirmwareReply
  | StatePowerReply
  | StateLabelReply
  | StateVersionReply
  | StateInfoReply
  | AcknowledgementReply
  | StateLocationReply
  | StateGroupReply
  | StateUnknown54Reply
  | EchoReply
  deriving (Show, Eq, Generic)

instance NFData DeviceReply where
  rnf
    = genericRnfV1

data LightReply
  = StateLightReply
  | StateInfraredReply
  | StateLightPowerReply
  deriving (Show, Eq, Generic)

instance NFData LightReply where
  rnf
    = genericRnfV1

data MultiZoneReply
  = StateZoneReply
  | StateMultiZoneReply
  deriving (Show, Eq, Generic)

instance NFData MultiZoneReply where
  rnf
    = genericRnfV1


newtype ByteId16
  = ByteId16 { unByte16 :: [Word8] }
  deriving (Show, Eq)

instance Binary ByteId16 where
  put
    = Bin.put . unByte16

  get
    = do
    unByte16 <- replicateM 16 BinG.getWord8
    pure ByteId16 {..}

newtype LocationId
  = LocationId { unLocationId :: ByteId16 }
  deriving (Show, Eq)
  deriving newtype Binary

instance Default LocationId where
  def
    = LocationId $ ByteId16 $ replicate 16 0

instance Default (Label n) where
  def
    = Label ""

newtype GroupId
  = GroupId { unGroupId :: ByteId16 }
  deriving (Show, Eq)
  deriving newtype Binary

instance Default GroupId where
  def
    = GroupId $ ByteId16 $ replicate 16 0



data Magic
  = O
  | I
  | S

data LifxProtocol (a :: Magic) where
  GetLocation' :: LifxProtocol 'O
  StateLocation' :: Int -> LifxProtocol 'S
  SetLocation' :: Int -> LifxProtocol 'S


instance WithSize (LifxProtocol a) where
  size GetLocation' = 0
  size StateLocation' {} = 4
  size SetLocation' {} = 4

deriving instance Eq (LifxProtocol a)


data GetLocation
  = GetLocation
  deriving Show

instance Binary GetLocation where
  put
    = const $ pure ()
  get
    = pure GetLocation

instance MessageId GetLocation where
  type StateReply GetLocation = StateLocation
  msgId = const 48
  msgTyp = const $ DeviceMessageType GetLocationMessage

newtype LifxUTC
  = LifxUTC { unLifxUTC :: Word64le }
  deriving Show

instance Binary LifxUTC where
  put
    = Bin.put . unLifxUTC
  get
    = LifxUTC <$> Bin.get

data StateLocation
  = StateLocation
  { stlLocation  :: !LocationId
  , stlLabel     :: !(Label "location")
  , stlUpdatedAt :: !LifxUTC
  }
  deriving Show

instance Binary StateLocation where
  put StateLocation {..}
    = do
    Bin.put stlLocation
    Bin.put stlLabel
    Bin.put stlUpdatedAt

  get
    = do
    stlLocation <- Bin.get
    stlLabel <- Bin.get
    stlUpdatedAt <- Bin.get
    pure StateLocation {..}

instance WithSize StateLocation where
  size
    = const (16 + 32 + 8)

instance WithSize GetLocation where
  size
    = const 0


data GetPower
  = GetPower
  deriving Show

instance Binary GetPower where
  put
    = const $ pure ()

  get
    = pure GetPower

data GetLightPower
  = GetLightPower
  deriving Show

instance Binary GetLightPower where
  put
    = const $ pure ()

  get
    = pure GetLightPower

instance MessageId GetLightPower where
  type StateReply GetLightPower = StateLightPower
  msgId = const 116
  msgTyp = const $ LightMessageType GetLightPowerMessage

newtype StateLightPower
  = StateLightPower
  { stlpLevel :: LightPower }
  deriving Show

instance Binary StateLightPower where
  put StateLightPower {..}
    = Bin.put stlpLevel

  get
    = do
    stlpLevel <- Bin.get
    pure StateLightPower {..}

instance WithSize StateLightPower where
  size
    = const 2

instance WithSize GetLightPower where
  size
    = const 0

data SetLightPower
  = SetLightPower
  { selpLevel    :: !LightPower
  , selpDuration :: !Word32le --ms
  }
  deriving Show

instance Binary SetLightPower where
  put SetLightPower {..}
    = do
    Bin.put selpLevel
    Bin.put selpDuration

  get
    = do
    selpLevel <- Bin.get
    selpDuration <- Bin.get
    pure SetLightPower {..}

instance MessageId SetLightPower where
  type StateReply SetLightPower = StateLightPower
  msgId = const 117
  msgTyp = const $ LightMessageType SetLightPowerMessage

instance WithSize SetLightPower where
  size
    = const 6

data SetWaveform
  = SetWaveform
  { sewReserved  :: !Word8
  , sewTransient :: !Word8  -- 8-bit integer as 0 or 1	Color does not persist.
  , sewColor     :: !HSBK  --Hsbk	Light end color.
  , sewPeriod    :: !Word32le  --unsigned 32-bit integer	Duration of a cycle in milliseconds.
  , sewCycles    :: !Float32le -- 32-bit float	Number of cycles.
  , sewSkewRatio :: !Int16le  --signed 16-bit integer	Waveform Skew, [-32768, 32767] scaled to [0, 1].
  , sewWaveform  :: !Word8  --unsigned 8-bit integer	Waveform to use for transition.
  }
  deriving Show

instance Binary SetWaveform where
  put SetWaveform {..}
    = do
    Bin.put sewReserved
    Bin.put sewTransient
    Bin.put sewColor
    Bin.put sewPeriod
    Bin.put sewCycles
    Bin.put sewSkewRatio
    Bin.put sewWaveform

  get
    = do
    sewReserved <- Bin.get
    sewTransient <- Bin.get
    sewColor <- Bin.get
    sewPeriod <- Bin.get
    sewCycles <- Bin.get
    sewSkewRatio <- Bin.get
    sewWaveform <- Bin.get
    pure SetWaveform {..}

instance MessageId SetWaveform where
  type StateReply SetWaveform = StateLight
  msgId = const 103
  msgTyp = const $ LightMessageType SetWaveformMessage

instance WithSize SetWaveform where
  size
    = const (1 + 1 + 8 + 4 + 4 + 2 + 1)


data SetWaveformOptional
  = SetWaveformOptional
  { sewoReserved      :: !Word8
  , sewoTransient     :: !Word8  -- 8-bit integer as 0 or 1	Color does not persist.
  , sewoColor         :: !HSBK  --Hsbk	Light end color.
  , sewoPeriod        :: !Word32le  --unsigned 32-bit integer	Duration of a cycle in milliseconds.
  , sewoCycles        :: !Float32le -- 32-bit float	Number of cycles.
  , sewoSkewRatio     :: !Int16le  --signed 16-bit integer	Waveform Skew, [-32768, 32767] scaled to [0, 1].
  , sewoWaveform      :: !Word8  --unsigned 8-bit integer	Waveform to use for transition.
  , sewoSetHue        :: !Word8  -- 8-bit integer as 0 or 1
  , sewoSetSaturation :: !Word8  -- 8-bit integer as 0 or 1
  , sewoSetBrightness :: !Word8  -- 8-bit integer as 0 or 1
  , sewoSetKelvin     :: !Word8  -- 8-bit integer as 0 or 1
  }
  deriving Show

instance Binary SetWaveformOptional where
  put SetWaveformOptional {..}
    = do
    Bin.put sewoReserved
    Bin.put sewoTransient
    Bin.put sewoColor
    Bin.put sewoPeriod
    Bin.put sewoCycles
    Bin.put sewoSkewRatio
    Bin.put sewoWaveform
    Bin.put sewoSetHue
    Bin.put sewoSetSaturation
    Bin.put sewoSetBrightness
    Bin.put sewoSetKelvin

  get
    = do
    sewoReserved <- Bin.get
    sewoTransient <- Bin.get
    sewoColor <- Bin.get
    sewoPeriod <- Bin.get
    sewoCycles <- Bin.get
    sewoSkewRatio <- Bin.get
    sewoWaveform <- Bin.get
    sewoSetHue <- Bin.get
    sewoSetSaturation <- Bin.get
    sewoSetBrightness <- Bin.get
    sewoSetKelvin <- Bin.get
    pure SetWaveformOptional {..}

instance MessageId SetWaveformOptional where
  type StateReply SetWaveformOptional = StateLight
  msgId = const 119
  msgTyp = const $ LightMessageType SetWaveformOptionalMessage

instance WithSize SetWaveformOptional where
  size
    = const (1 + 1 + 8 + 4 + 4 + 2 + 1 + 1 + 1 + 1 + 1)

data GetLight
  = GetLight
  deriving Show

instance Binary GetLight where
  put
    = const $ pure ()

  get
    = pure GetLight

instance MessageId GetLight where
  type StateReply GetLight = StateLight
  msgId = const 101
  msgTyp = const $ LightMessageType GetLightMessage

data StateLight
  = StateLight
  { stliColor     :: !HSBK
  , stliReserved  :: !Int16
  , stliPower     :: !LightPower
  , stliLabel     :: !(Label "name")
  , stliReserved2 :: !Word64le
  }
  deriving Show

newtype LightPower
  = LightPower
  { unLightPower :: Word16le }
  deriving (Show, Eq)

instance Binary LightPower where
  put
    = Bin.put . unLightPower

  get
    = LightPower <$> Bin.get

instance Binary StateLight where
  put StateLight {..}
    = do
    Bin.put stliColor
    BinP.putInt16le stliReserved
    Bin.put stliPower
    Bin.put stliLabel
    Bin.put stliReserved2

  get
    = do
    stliColor <- Bin.get
    stliReserved <- BinG.getInt16le
    stliPower <- Bin.get
    stliLabel <- Bin.get
    stliReserved2 <- Bin.get
    pure StateLight {..}

instance WithSize StateLight where
  size
    = const $ 8 + (2 + 2 + 32 + 8)

instance WithSize GetLight where
  size
    = const 0


data GetInfrared
  = GetInfrared
  deriving Show

instance Binary GetInfrared where
  put
    = const $ pure ()

  get
    = pure GetInfrared

instance MessageId GetInfrared where
  type StateReply GetInfrared = StateInfrared
  msgId = const 120
  msgTyp = const $ LightMessageType GetInfraredMessage

instance WithSize GetInfrared where
  size
    = const 0

newtype StateInfrared
  = StateInfrared
  { stiBrightness :: Word16le }
  deriving Show

instance Binary StateInfrared where
  put
    = Bin.put . stiBrightness

  get
    = do
    stiBrightness <- Bin.get
    pure StateInfrared {..}

instance WithSize StateInfrared where
  size
    = const 2

newtype SetInfrared
  = SetInfrared
  { seiBrightness :: Word16le }
  deriving Show

instance Binary SetInfrared where
  put
    = Bin.put . seiBrightness

  get
    = SetInfrared <$> Bin.get

instance WithSize SetInfrared where
  size
    = const 2


data GetLabel
  = GetLabel
  deriving Show

instance Binary GetLabel where
  put
    = const $ pure ()

  get
    = pure GetLabel

instance MessageId GetLabel where
  type StateReply GetLabel = StateLabel
  msgId = const 48
  msgTyp = const $ DeviceMessageType GetLabelMessage

newtype StateLabel
  = StateLabel
  { stlaLabel :: Label "name" }
  deriving Show

instance Binary StateLabel where
  put
    = Bin.put . stlaLabel

  get
    = do
    stlaLabel <- Bin.get
    pure StateLabel {..}

instance WithSize StateLabel where
  size
    = const 32

instance WithSize GetLabel where
  size
    = const 0

newtype VendorId
  = VendorId Word32le
  deriving (Show, Eq, Binary)

instance Default VendorId where
  def
    = VendorId 1

newtype HardwareVersion
  = HardwareVersion Word32le
  deriving (Show, Eq, Binary)


data ProductId
  = Original1000
  | Color650
  | White800lv
  | White800hv
  | White900BR30lv
  | Color1000BR30
  | Color1000
  | LIFXA19
  | LIFXBR30
  | LIFXPA19
  | LIFXPBR30
  | LIFXZ
  | LIFXZ2
  | LIFXDownlight
  | LIFXDownlight_
  | LIFXA19_
  | LIFXBR30_
  | LIFXPA19_
  | LIFXPBR30_
  | LIFXMini
  | LIFXMiniDaD
  | LIFXMiniWhite
  | LIFXGU10
  deriving (Show, Eq)

instance Binary ProductId where
  put = \case
    Original1000 -> putWord32le 1
    Color650 -> putWord32le 3
    White800lv -> putWord32le 10
    White800hv -> putWord32le 11
    White900BR30lv -> putWord32le 18
    Color1000BR30 -> putWord32le 20
    Color1000 -> putWord32le 22
    LIFXA19 -> putWord32le 27
    LIFXBR30 -> putWord32le 28
    LIFXPA19 -> putWord32le 29
    LIFXPBR30 -> putWord32le 30
    LIFXZ -> putWord32le 31
    LIFXZ2 -> putWord32le 32
    LIFXDownlight -> putWord32le 36
    LIFXDownlight_ -> putWord32le 37
    LIFXA19_ -> putWord32le 43
    LIFXBR30_ -> putWord32le 44
    LIFXPA19_ -> putWord32le 45
    LIFXPBR30_ -> putWord32le 46
    LIFXMini -> putWord32le 49
    LIFXMiniDaD -> putWord32le 50
    LIFXMiniWhite -> putWord32le 51
    LIFXGU10 -> putWord32le 52
    where
      putWord32le
        :: Word32le
        -> Put
      putWord32le = Bin.put
  get = maybe (fail "not a known product") pure =<< (getWord32le <$> Bin.get)
    where
      getWord32le
        :: Word32le
        -> Maybe ProductId
      getWord32le = \case
        1 -> Just Original1000
        3 -> Just Color650
        10 -> Just White800lv
        11 -> Just White800hv
        18 -> Just White900BR30lv
        20 -> Just Color1000BR30
        22 -> Just Color1000
        27 -> Just LIFXA19
        28 -> Just LIFXBR30
        29 -> Just LIFXPA19
        30 -> Just LIFXPBR30
        31 -> Just LIFXZ
        32 -> Just LIFXZ2
        36 -> Just LIFXDownlight
        37 -> Just LIFXDownlight_
        43 -> Just LIFXA19_
        44 -> Just LIFXBR30_
        45 -> Just LIFXPA19_
        46 -> Just LIFXPBR30_
        49 -> Just LIFXMini
        50 -> Just LIFXMiniDaD
        51 -> Just LIFXMiniWhite
        52 -> Just LIFXGU10
        _ -> Nothing


data StateVersion
  = StateVersion
  { stvVendor  :: !VendorId
  , stvProduct :: !ProductId
  , stvVersion :: !HardwareVersion
  }
  deriving Show

instance Binary StateVersion where
  put StateVersion {..}
    = do
    Bin.put stvVendor
    Bin.put stvProduct
    Bin.put stvVersion

  get
    = do
    stvVendor <- Bin.get
    stvProduct <- Bin.get
    stvVersion <- Bin.get
    pure StateVersion {..}

instance WithSize StateVersion where
  size
    = const 12

data GetVersion
  = GetVersion
  deriving Show

instance Binary GetVersion where
  put
    = const $ pure ()
  get
    = pure GetVersion

instance MessageId GetVersion where
  type StateReply GetVersion = StateVersion
  msgId = const 32
  msgTyp = const $ DeviceMessageType GetVersionMessage

instance WithSize GetVersion where
  size
    = const 0

data GetHostInfo
  = GetHostInfo
  deriving Show

instance Binary GetHostInfo where
  put
    = const $ pure ()
  get
    = pure GetHostInfo

instance MessageId GetHostInfo where
  type StateReply GetHostInfo = StateHostInfo
  msgId = const 12
  msgTyp = const $ DeviceMessageType GetHostInfoMessage

instance WithSize GetHostInfo where
  size
    = const 0

data StateHostInfo
  = StateHostInfo
  { sthiSignal   :: !Float32le
  , sthiTx       :: !Word32le
  , sthiRx       :: !Word32le
  , sthiReserved :: !Int16le
  }
  deriving Show

instance Binary StateHostInfo where
  put StateHostInfo {..}
    = do
    Bin.put sthiSignal
    Bin.put sthiTx
    Bin.put sthiRx
    Bin.put sthiReserved
  get
    = do
    sthiSignal <- Bin.get
    sthiTx <- Bin.get
    sthiRx <- Bin.get
    sthiReserved <- Bin.get
    pure StateHostInfo {..}

instance WithSize StateHostInfo where
  size
    = const $ 4 + 4 + 4 + 2


data GetHostFirmware
  = GetHostFirmware
  deriving Show

instance Binary GetHostFirmware where
  put
    = const $ pure ()
  get
    = pure GetHostFirmware

instance MessageId GetHostFirmware where
  type StateReply GetHostFirmware = StateHostFirmware
  msgId = const 14
  msgTyp = const $ DeviceMessageType GetHostFirmwareMessage

instance WithSize GetHostFirmware where
  size
    = const 0

data StateHostFirmware
 = StateHostFirmware
  { sthfBuild    :: !Word64le
  , sthfReserved :: !Word64le
  , sthfVersion  :: !Word32le
  }
  deriving Show

instance Binary StateHostFirmware where
  put StateHostFirmware {..}
    = do
    Bin.put sthfBuild
    Bin.put sthfReserved
    Bin.put sthfVersion

  get
    = do
    sthfBuild <- Bin.get
    sthfReserved <- Bin.get
    sthfVersion <- Bin.get
    pure StateHostFirmware {..}

instance WithSize StateHostFirmware where
  size
    = const $ 8 + 8 + 4


data GetWifiInfo
  = GetWifiInfo
  deriving Show

instance Binary GetWifiInfo where
  put
    = const $ pure ()
  get
    = pure GetWifiInfo

instance MessageId GetWifiInfo where
  type StateReply GetWifiInfo = StateWifiInfo
  msgId = const 16
  msgTyp = const $ DeviceMessageType GetWifiInfoMessage

instance WithSize GetWifiInfo where
  size
    = const 0

data StateWifiInfo
  = StateWifiInfo
  { stwiSignal   :: !Float32le
  , stwiTx       :: !Word32le
  , stwiRx       :: !Word32le
  , stwiReserved :: !Int16le
  }
  deriving Show

instance Binary StateWifiInfo where
  put StateWifiInfo {..}
    = do
    Bin.put stwiSignal
    Bin.put stwiTx
    Bin.put stwiRx
    Bin.put stwiReserved
  get
    = do
    stwiSignal <- Bin.get
    stwiTx <- Bin.get
    stwiRx <- Bin.get
    stwiReserved <- Bin.get
    pure StateWifiInfo {..}

instance WithSize StateWifiInfo where
  size
    = const $ 4 + 4 + 4 + 2

data GetWifiFirmware
  = GetWifiFirmware
  deriving Show

instance Binary GetWifiFirmware where
  put
    = const $ pure ()
  get
    = pure GetWifiFirmware

instance MessageId GetWifiFirmware where
  type StateReply GetWifiFirmware = StateWifiFirmware
  msgId = const 18
  msgTyp = const $ DeviceMessageType GetWifiFirmwareMessage

instance WithSize GetWifiFirmware where
  size
    = const 0

data StateWifiFirmware
  = StateWifiFirmware
  { stwfBuild    :: !Word64le -- ns since epoch
  , stwfReserved :: !Word64le
  , stwfVersion  :: !Word32le
  } deriving Show

instance Binary StateWifiFirmware where
  put StateWifiFirmware {..}
    = do
    Bin.put stwfBuild
    Bin.put stwfReserved
    Bin.put stwfVersion
  get
    = do
    stwfBuild <- Bin.get
    stwfReserved <- Bin.get
    stwfVersion <- Bin.get
    pure StateWifiFirmware {..}

instance WithSize StateWifiFirmware where
  size
    = const $ 8 + 8 + 4

data GetInfo
  = GetInfo
  deriving Show

instance Binary GetInfo where
  put
    = const $ pure ()
  get
    = pure GetInfo

instance WithSize GetInfo where
  size
    = const 0

data StateInfo
  = StateInfo
  { stiTime     :: !Word64le
  , stiUptime   :: !Word64le
  , stiDowntime :: !Word64le
  }
  deriving Show

instance Binary StateInfo where
  put StateInfo {..}
    = do
    Bin.put stiTime
    Bin.put stiUptime
    Bin.put stiDowntime

  get
    = do
    stiTime <- Bin.get
    stiUptime <- Bin.get
    stiDowntime <- Bin.get
    pure StateInfo {..}

instance WithSize StateInfo where
  size
    = const $ 8 + 8 + 8

data GetUnknown54
  = GetUnknown54
  deriving Show

instance Binary GetUnknown54 where
  put
    = const $ pure ()
  get
    = pure GetUnknown54

instance MessageId GetUnknown54 where
  type StateReply GetUnknown54 = StateUnknown54
  msgId = const 54
  msgTyp = const $ DeviceMessageType GetUnknown54Message

instance WithSize GetUnknown54 where
  size
    = const 0

newtype Unknown54Id
  = Unknown54Id { unUnknown54Id :: ByteId16 }
  deriving (Show, Eq, Binary)

instance Default Unknown54Id where
  def
    = Unknown54Id $ ByteId16 $ replicate 16 0

data StateUnknown54
  = StateUnknown54
  { stu54Unknown54Id :: !Unknown54Id
  , stu54Label       :: !(Label "unknown54")
  , stu54UpdatedAt   :: !LifxUTC
  }
  deriving Show

instance Binary StateUnknown54 where
  put StateUnknown54 {..}
    = do
    Bin.put stu54Unknown54Id
    Bin.put stu54Label
    Bin.put stu54UpdatedAt

  get
    = do
    stu54Unknown54Id <- Bin.get
    stu54Label <- Bin.get
    stu54UpdatedAt <- Bin.get
    pure StateUnknown54 {..}

instance WithSize StateUnknown54 where
  size
    = const $ 16 + 32 + 8


data GetGroup
  = GetGroup
  deriving Show

instance Binary GetGroup where
  put
    = const $ pure ()
  get
    = pure GetGroup

instance MessageId GetGroup where
  type StateReply GetGroup = StateGroup
  msgId = const 51
  msgTyp = const $ DeviceMessageType GetGroupMessage

data StateGroup
  = StateGroup
  { stgGroup     :: !GroupId
  , stgLabel     :: !(Label "group")
  , stgUpdatedAt :: !LifxUTC
  }
  deriving Show

instance Binary StateGroup where
  put StateGroup {..}
    = do
    Bin.put stgGroup
    Bin.put stgLabel
    Bin.put stgUpdatedAt

  get
    = do
    stgGroup <- Bin.get
    stgLabel <- Bin.get
    stgUpdatedAt <- Bin.get
    pure StateGroup {..}

instance WithSize StateGroup where
  size
    = const (16 + 32 + 8)

instance WithSize GetGroup where
  size
    = const 0

newtype SetPower
  = SetPower
  { spLevel :: Word16le }
  deriving Show

instance Binary SetPower where
  put
    = Bin.put . spLevel
  get
    = SetPower <$> Bin.get

newtype StatePower
  = StatePower
  { stpLevel :: Word16le }
  deriving Show

instance Binary StatePower where
  put
    = Bin.put . stpLevel
  get
    = StatePower <$> Bin.get

newtype SetLabel
  = SetLabel
  { selLabel :: Label "name" }
  deriving Show

instance Binary SetLabel where
  put
    = Bin.put . selLabel
  get
    = SetLabel <$> Bin.get

instance WithSize SetLabel where
  size
    = const 32

data State
  = State
  { sColor     :: !HSBK
  , sReserved  :: !Int16le
  , sPower     :: !Word16le
  , sLabel     :: !(Label "name")
  , sReserved2 :: !Word64le
  }
  deriving Show

instance Binary State where
  put State {..}
    = do
    Bin.put sColor
    Bin.put sReserved
    Bin.put sPower
    Bin.put sLabel
    Bin.put sReserved2
  get
    = State
    <$> Bin.get
    <*> Bin.get
    <*> Bin.get
    <*> Bin.get
    <*> Bin.get

data Acknowledgement
  = Acknowledgement
  deriving Show

instance Binary Acknowledgement where
  put
    = const $ pure ()
  get
    = pure Acknowledgement

instance WithSize Acknowledgement where
  size = const 0


-- 102: Layout in bits:   8|_|32
--                        ui|hsbk|ui
data SetColor
  = SetColor
  { secReserved :: !()
  , secColor    :: !HSBK
  , secDuration :: !Word32le -- ms
  }
  deriving Show

instance Binary SetColor where
  put _sc@SetColor {..}
    = do
    BinP.putWord8 0
    Bin.put secColor
    Bin.put secDuration

  get
    = do
    secReserved <- () <$ BinG.getWord8
    secColor <- Bin.get
    secDuration <- Bin.get
    pure SetColor {..}

instance MessageId SetColor where
  type StateReply SetColor = StateLight
  msgId = const 102
  msgTyp = const $ LightMessageType SetColorMessage

instance WithSize SetColor where
  size SetColor {..}
    = 1 + size secColor + 4


data GetService
  = GetService
  deriving (Show, Eq)

data StateService
  = StateService
  { ssService :: !Word8
  , ssPort    :: !Word32le
  }
  deriving Show

instance MessageId GetService where
  type StateReply GetService = StateService
  msgId = const 2
  msgTyp = const $ DeviceMessageType GetServiceMessage


newtype UniqueSource
  = UniqueSource { unUniqueSource :: Word32le }
  deriving (Show, Eq, Generic)

instance NFData UniqueSource where
  rnf
    = genericRnfV1

newtype Target
  = Target { unTarget :: Mac Word8 {-Word64-} }
  deriving (Eq, Generic)

instance Show Target where
  show (Target t)
    = printMac t

instance NFData Target where
  rnf
    = genericRnfV1

newtype Sequence
  = Sequence { unSequence :: Word8 }
  deriving (Show, Eq, Generic)

instance NFData Sequence where
  rnf
    = genericRnfV1



class WithSize a where
  size :: a -> Word16le

instance WithSize a => WithSize (Packet a) where
  size Packet {..}
    = size pFrame + size pFrameAddress + size pProtocolHeader + size pPayload

instance WithSize Frame where
  size
    = const 8

instance WithSize FrameAddress where
  size
    = const 16

instance WithSize ProtocolHeader where
  size
    = const 12

instance WithSize HSBK where
  size
    = const 8

instance WithSize GetService where
  size _
    = 0

instance WithSize StateService where
  size _
    = 5

instance Binary Target where
  put t
    = Bin.put $ targetToWord64le t
  get
    = word64leToTarget <$> Bin.get


data Header
  = Header
  { hFrame          :: !Frame
  , hFrameAddress   :: !FrameAddress
  , hProtocolHeader :: !ProtocolHeader
  }
  deriving (Show, Generic)

instance NFData Header where
  rnf
    = genericRnfV1


data CallbackWrap
  = forall a. (Show a, Binary a, WithSize a) =>
  CallbackWrap
  { runDecode   :: !(Header -> BSL.ByteString -> Except PayloadDecodeError (Packet a))
  , runCallback :: !(Callback a)
  }

type Callback a
  = (SharedState -> Packet a -> SockAddr -> BSL.ByteString -> IO ())

instance Show CallbackWrap where
  show
    = const "Callback..."

data AppState
  = AppState
  { asSharedState     :: !SharedState
  , asReceiveThread   :: !(Async ())
  , asDiscoveryThread :: !(Async ())
  }

data SharedState
  = SharedState
  { ssReplyCallbacks :: !(TArray Word8 CallbackWrap)
  , ssDevices        :: !(TVar (HM.HashMap DeviceId Light))
  , ssSocket         :: !Socket
  , ssNextSeq        :: !(IO Sequence)
  , ssUniqueSource   :: !UniqueSource
  , ssLogLevel       :: !LogLevel
  , ssLogFunction    :: !(LogLevel -> String -> IO ())
  }

data Light
  = Light
  { lDevice                 :: !Device
  , lGroup                  :: !(Maybe (Label "group"))
  , lLocation               :: !(Maybe (Label "location"))
  , lLabel                  :: !(Maybe (Label "name"))
  , lColor                  :: !(Maybe HSBK)
  , lPower                  :: !(Maybe LightPower)
  , lProduct                :: !(Maybe ProductId)
  , lHardwareVersion        :: !(Maybe HardwareVersion)
  , lHostFirmwareBuild      :: !(Maybe Word64le)
  , lHostFirmwareVersion    :: !(Maybe Word32le)
  , lWifiFirmwareBuild      :: !(Maybe Word64le)
  , lWifiFirmwareVersion    :: !(Maybe Word32le)
  }
  deriving (Show, Eq)

newtype Mac a
  = Mac { unMac :: (a, a, a, a, a, a) }
  deriving (Show, Eq, Hashable, Generic)

instance NFData a => NFData (Mac a) where
  rnf
    = genericRnfV1

instance Functor Mac where
  fmap g (Mac (a,b,c,d,e,f))
    = Mac (g a, g b, g c, g d, g e, g f)


newtype DeviceId
  = DeviceId (Mac Word8)
  deriving (Eq, Hashable)

instance Show DeviceId where
  show (DeviceId t)
    = printMac t

newtype DeviceAddress
  = DeviceAddress Word32le
  deriving (Show, Eq)

data DeviceSocketAddress
  = DeviceSocketAddress !PortNumber !DeviceAddress
  deriving (Eq)

instance Show DeviceSocketAddress where
  show (DeviceSocketAddress p (DeviceAddress w))
    = "DeviceSocketAddress " <> show p <> " IP " <> show (hostAddressToTuple $ unWord32le w)

data Device
  = Device
  { dAddr     :: !DeviceSocketAddress
  , dDeviceId :: !DeviceId
  }
  deriving (Show, Eq)

data HeaderDecodeError
  = NotAHeader
  { hdeError     :: !String
  , hdeOrig      :: !BSL.ByteString
  , hdeRemaining :: !BSL.ByteString
  , hdeOffset    :: !BinG.ByteOffset
  }
  | ImproperSourceInHeader
  { hdeHeader    :: !Header
  , hdeOrig      :: !BSL.ByteString
  , hdeRemaining :: !BSL.ByteString
  , hdeOffset    :: !BinG.ByteOffset
  }
  | ImproperSizeInHeader
  { hdeHeader    :: !Header
  , hdeOrig      :: !BSL.ByteString
  , hdeRemaining :: !BSL.ByteString
  , hdeOffset    :: !BinG.ByteOffset
  }
  deriving (Show, Generic)

instance Exception HeaderDecodeError

instance NFData HeaderDecodeError where
  rnf
    = genericRnfV1

data PayloadDecodeError
  = PayloadDecodeFailed
  { pdeHeader             :: !Header
  , pdeRemaining          :: !BSL.ByteString
  , pdeRemainingAfterFail :: !BSL.ByteString
  , pdeOffsetAfterFail    :: !BinG.ByteOffset
  , pdeError              :: !String
  }
  deriving Show


type MessageIdC c
  = ( WithSize c
    , MessageId c
    , Binary c
    , Show c
    , Show (StateReply c)
    , Binary (StateReply c)
    , WithSize (StateReply c)
    )


data FoundDevice
  = FoundDevice
  {
  }

data LogLevel
  = LogDebug
  | LogInfo
  | LogError
  deriving (Eq, Ord, Show)

instance Binary UniqueSource where
  put (UniqueSource t)
    = Bin.put t
  get
    = UniqueSource <$> Bin.get

instance Binary Sequence where
  put (Sequence t)
    = BinP.putWord8 t
  get
    = Sequence <$> BinG.getWord8

instance Binary Frame where
  put f@Frame {..}
    = do
    Bin.put fSize -- Discovered from size of rest
    putFrame2ndByte f
    Bin.put fSource

  get
    = do
    fSize <- Bin.get
    frame2ndByte <- Bin.get @Word16le
    let
      fProtocol = extract frame2ndByte 0 11
      fOrigin = extract frame2ndByte 14 16
      fAddressable = boolToAddressable $ testBit frame2ndByte 12
      fTagged = boolToTagged $ testBit frame2ndByte 13


    fSource <- Bin.get
    pure Frame {..}

putFrame2ndByte
  :: Frame
  -> Put
putFrame2ndByte Frame {..}
  = Bin.put @Word16le $
  (fProtocol `shiftL` 0) +
  (bool 0 1 (addressableToBool fAddressable) `shiftL` 12) +
  (bool 0 1 (taggedToBool fTagged) `shiftL` 13) +
  fromIntegral (fOrigin `shiftL` 14)

instance Binary FrameAddress where
  put _f@FrameAddress {..}
    = do
    Bin.put faTarget
    Bin.put faReserved
    BinP.putWord8 $
      (bool 0 1 (resRequiredToBool faResRequired) `shiftL` 0) +
      (bool 0 1 (ackRequiredToBool faAckRequired) `shiftL` 1) +
      (0 `shiftL` 2)
    Bin.put faSequence

  get
    = do
    faTarget <- Bin.get
    faReserved <- Bin.get
    frameAddress15thByte <- BinG.getWord8
    let
      _faReserved2 :: Word8
      _faReserved2 = extract frameAddress15thByte 2 7
      faReserved2 = ()
      faResRequired = boolToResRequired $ testBit frameAddress15thByte 0
      faAckRequired = boolToAckRequired $ testBit frameAddress15thByte 1
    faSequence <- Bin.get
    pure FrameAddress {..}

instance Binary UnusedMac where
  put _
    = replicateM_ 6 $ BinP.putWord8 0

  get
    = do
    void $ replicateM_ 6 BinG.getWord8
    pure $ UnusedMac $ Mac ((), (), (), (), (), ())

instance Binary ProtocolHeader where
  put _p@ProtocolHeader {..}
    = do
      Bin.put @Word64le 0
      Bin.put $ directionToWord16le phType
      Bin.put @Word16le 0

  get
    = do
    _ <- Bin.get @Word64le
    phType_ <- word16leToDirection <$> Bin.get
    phType <- case phType_ of
      Left p -> fail $ show p
      Right p -> pure p --TODO FIxme

    _ <- Bin.get @Word16le

    pure ProtocolHeader {phReserved = 0,phReserved2 = (),..}

instance Binary HSBK where
  put _hsbk@HSBK {..}
    = do
    Bin.put hsbkHue
    Bin.put hsbkSaturation
    Bin.put hsbkBrightness
    Bin.put hsbkKelvin

  get
    = do
    hsbkHue <- Bin.get
    hsbkSaturation <- Bin.get
    hsbkBrightness <- Bin.get
    hsbkKelvin <- Bin.get
    pure HSBK {..}

instance Binary GetService where
  put _
    = pure ()
  get
    = pure GetService

instance Binary StateService where
  put StateService {..}
    = do
    BinP.putWord8 ssService
    Bin.put ssPort

  get
    = do
    ssService <- BinG.getWord8
    when (ssService /= 1)
      $ fail
      $ "Not a StateService mismatched service: " <> show ssService

    ssPort <- Bin.get
    pure StateService {..}

instance Binary Header where
  put Header {..}
    = do
    Bin.put hFrame
    Bin.put hFrameAddress
    Bin.put hProtocolHeader

  get
    = do
    hFrame <- Bin.get
    hFrameAddress <- Bin.get
    hProtocolHeader <- Bin.get
    pure Header {..}


packetFromHeader
  :: ( Binary a
     , WithSize a
     )
  => Header
  -> a
  -> Packet a
packetFromHeader Header {..}
  = Packet hFrame hFrameAddress hProtocolHeader



taggedToBool
  :: Tagged
  -> Bool
taggedToBool SingleTagged
  = False
taggedToBool AllTagged
  = True

addressableToBool
  :: Addressable
  -> Bool
addressableToBool NoFrameAddress
  = False
addressableToBool HasFrameAddress
  = True

ackRequiredToBool
  :: AckRequired
  -> Bool
ackRequiredToBool NoAckRequired
  = False
ackRequiredToBool AckRequired
  = True

resRequiredToBool
  :: ResRequired
  -> Bool
resRequiredToBool NoResRequired
  = False
resRequiredToBool ResRequired
  = True


boolToTagged
  :: Bool
  -> Tagged
boolToTagged False
  = SingleTagged
boolToTagged True
  = AllTagged

boolToAddressable
  :: Bool
  -> Addressable
boolToAddressable False
  = NoFrameAddress
boolToAddressable True
  = HasFrameAddress

boolToAckRequired
  :: Bool
  -> AckRequired
boolToAckRequired False
  = NoAckRequired
boolToAckRequired True
  = AckRequired

boolToResRequired
  :: Bool
  -> ResRequired
boolToResRequired False
  = NoResRequired
boolToResRequired True
  = ResRequired


mkLabel
  :: TL.Text
  -> Label n
mkLabel = Label . TL.dropAround (not . isPrint) . TL.strip

extractByte
  :: ( Bits a
     , Integral a
     , Integral b
     )
  => a
  -> Int
  -> b
extractByte x t
  = fromIntegral $ (x .&. (0xFF `shiftL` tt)) `shiftR` tt
  where
    tt = t * 8

printMac
  :: ( Num a
     , Integral a
     , Show a
     , PrintfArg a
     )
  => Mac a
  -> String
printMac m
  = show m <> "( " <> hex m <> " )"
  where
    hex (fmap hexS -> unMac -> (s1, s2, s3, s4, s5, s6))
      = s1
      <> ":"
      <> s2
      <> ":"
      <> s3
      <> ":"
      <> s4
      <> ":"
      <> s5
      <> ":"
      <> s6
    hexS = printf "%02x"

targetToWord64le
  :: Target
  -> Word64le
targetToWord64le (Target (Mac t))
  =
  let
    (b1, b2, b3, b4, b5, b6) = t

    r8 = fromIntegral b6 `shiftL` 40
    r7 = fromIntegral b5 `shiftL` 32
    r6 = fromIntegral b4 `shiftL` 24
    r5 = fromIntegral b3 `shiftL` 16
    r4 = fromIntegral b2 `shiftL` 8
    r3 = fromIntegral b1 `shiftL` 0
  in
    r8 + r7 + r6 + r5 + r4 + r3

word64leToTarget
  :: Word64le
  -> Target
word64leToTarget n
  =
  let
    b6 = extractByte n 5
    b5 = extractByte n 4
    b4 = extractByte n 3
    b3 = extractByte n 2
    b2 = extractByte n 1
    b1 = extractByte n 0
  in
    Target $ Mac (b1, b2, b3, b4, b5, b6)
--
-- | Cutting out from M to N becomes a two-step process: you shift the original value M bits to the right, and then perform a bit-wise AND with the mask of N-M ones.
extract
  :: ( Integral a
     , Bits a
     , Integral b
     )
  => a
  -> Int
  -> Int
  -> b
extract x m n
  = fst $ extract' x m n

extract'
  :: ( Integral a
     , Bits a
     , Integral b
     )
  => a
  -> Int
  -> Int
  -> (b, a)
extract' x m n
  = (fromIntegral extracted, shifted)
  where
    extracted = shifted .&. bmask
    shifted = x `shiftR` m
    bmask = bit w - 1
    w = n - m



directionToWord16le
  :: Direction
  -> Word16le
directionToWord16le
  = \case
  Request rt -> messageTypeToWord16le rt
  Reply rt -> replyTypeToWord16le rt

messageTypeToWord16le
  :: MessageType
  -> Word16le
messageTypeToWord16le
 = \case
 DeviceMessageType dm ->
   deviceMessageTypeToWord16le dm
 LightMessageType lm ->
   lightMessageTypeToWord16le lm
 MultiZoneMessageType mzm ->
   multiZoneMessageTypeToWord16le mzm
 where
  deviceMessageTypeToWord16le = \case
    GetServiceMessage -> 2
    GetHostInfoMessage -> 12
    GetHostFirmwareMessage -> 14
    GetWifiInfoMessage -> 16
    GetWifiFirmwareMessage -> 18
    GetPowerMessage -> 20
    SetPowerMessage -> 21
    GetLabelMessage -> 23
    SetLabelMessage -> 24
    GetVersionMessage -> 32
    GetInfoMessage -> 34
    GetLocationMessage -> 48
    SetLocationMessage -> 49
    GetGroupMessage -> 51
    SetGroupMessage -> 52
    GetUnknown54Message -> 54
    EchoMessage -> 58
  lightMessageTypeToWord16le = \case
    GetLightMessage -> 101
    SetColorMessage -> 102
    SetWaveformMessage -> 103
    SetWaveformOptionalMessage -> 119
    GetLightPowerMessage -> 116
    SetLightPowerMessage -> 117
    GetInfraredMessage -> 120
    SetInfraredMessage -> 122
  multiZoneMessageTypeToWord16le = \case
    SetColorZonesMessage -> 501
    GetColorZonesMessage -> 502

replyTypeToWord16le
  :: ReplyType
  -> Word16le
replyTypeToWord16le
 = \case
 DeviceReplyType dm ->
   deviceReplyTypeToWord16le dm
 LightReplyType lm ->
   lightReplyTypeToWord16le lm
 MultiZoneReplyType mzm ->
   multiZoneReplyTypeToWord16le mzm
 where
  deviceReplyTypeToWord16le = \case
    StateServiceReply -> 3
    StateHostInfoReply -> 13
    StateHostFirmwareReply -> 15
    StateWifiInfoReply -> 17
    StateWifiFirmwareReply -> 19
    StatePowerReply -> 22
    StateLabelReply -> 25
    StateVersionReply -> 33
    StateInfoReply -> 35
    AcknowledgementReply -> 45
    StateLocationReply -> 50
    StateGroupReply -> 53
    StateUnknown54Reply -> 55
    EchoReply -> 59
  lightReplyTypeToWord16le = \case
    StateLightReply -> 107
    StateLightPowerReply -> 118
    StateInfraredReply -> 121
  multiZoneReplyTypeToWord16le = \case
    StateZoneReply -> 503
    StateMultiZoneReply -> 506

word16leToDirection
  :: Word16le
  -> Either String Direction
word16leToDirection w
  = (Request <$> word16leToMessageType w) <|> (Reply <$> word16leToReplyType w)

word16leToMessageType
  :: Word16le
  -> Either String MessageType
word16leToMessageType 2
  = Right $ DeviceMessageType GetServiceMessage
word16leToMessageType 12
  = Right $ DeviceMessageType GetHostInfoMessage
word16leToMessageType 14
  = Right $ DeviceMessageType GetHostFirmwareMessage
word16leToMessageType 16
  = Right $ DeviceMessageType GetWifiInfoMessage
word16leToMessageType 18
  = Right $ DeviceMessageType GetWifiFirmwareMessage
word16leToMessageType 20
  = Right $ DeviceMessageType GetPowerMessage
word16leToMessageType 21
  = Right $ DeviceMessageType SetPowerMessage
word16leToMessageType 23
  = Right $ DeviceMessageType GetLabelMessage
word16leToMessageType 24
  = Right $ DeviceMessageType SetLabelMessage
word16leToMessageType 32
  = Right $ DeviceMessageType GetVersionMessage
word16leToMessageType 34
  = Right $ DeviceMessageType GetInfoMessage
word16leToMessageType 48
  = Right $ DeviceMessageType GetLocationMessage
word16leToMessageType 49
  = Right $ DeviceMessageType SetLocationMessage
word16leToMessageType 51
  = Right $ DeviceMessageType GetGroupMessage
word16leToMessageType 52
  = Right $ DeviceMessageType SetGroupMessage
word16leToMessageType 54
  = Right $ DeviceMessageType GetUnknown54Message
word16leToMessageType 58
  = Right $ DeviceMessageType EchoMessage

word16leToMessageType 101
  = Right $ LightMessageType GetLightMessage
word16leToMessageType 102
  = Right $ LightMessageType SetColorMessage
word16leToMessageType 103
  = Right $ LightMessageType SetWaveformMessage
word16leToMessageType 119
  = Right $ LightMessageType SetWaveformOptionalMessage
word16leToMessageType 116
  = Right $ LightMessageType GetLightPowerMessage
word16leToMessageType 117
  = Right $ LightMessageType SetLightPowerMessage
word16leToMessageType 121
  = Right $ LightMessageType GetInfraredMessage
word16leToMessageType 122
  = Right $ LightMessageType SetInfraredMessage

word16leToMessageType 501
  = Right $ MultiZoneMessageType SetColorZonesMessage
word16leToMessageType 502
  = Right $ MultiZoneMessageType GetColorZonesMessage

word16leToMessageType x
  = Left $ "no case for " <> show x

word16leToReplyType
  :: Word16le
  -> Either String ReplyType
word16leToReplyType 3
  = Right $ DeviceReplyType StateServiceReply
word16leToReplyType 13
  = Right $ DeviceReplyType StateHostInfoReply
word16leToReplyType 15
  = Right $ DeviceReplyType StateHostFirmwareReply
word16leToReplyType 17
  = Right $ DeviceReplyType StateWifiInfoReply
word16leToReplyType 19
  = Right $ DeviceReplyType StateWifiFirmwareReply
word16leToReplyType 22
  = Right $ DeviceReplyType StatePowerReply
word16leToReplyType 25
  = Right $ DeviceReplyType StateLabelReply
word16leToReplyType 33
  = Right $ DeviceReplyType StateVersionReply
word16leToReplyType 35
  = Right $ DeviceReplyType StateInfoReply
word16leToReplyType 45
  = Right $ DeviceReplyType AcknowledgementReply
word16leToReplyType 50
  = Right $ DeviceReplyType StateLocationReply
word16leToReplyType 53
  = Right $ DeviceReplyType StateGroupReply
word16leToReplyType 55
  = Right $ DeviceReplyType StateUnknown54Reply
word16leToReplyType 59
  = Right $ DeviceReplyType EchoReply

word16leToReplyType 107
  = Right $ LightReplyType StateLightReply
word16leToReplyType 118
  = Right $ LightReplyType StateLightPowerReply
word16leToReplyType 121
  = Right $ LightReplyType StateInfraredReply

word16leToReplyType 503
  = Right $ MultiZoneReplyType StateZoneReply
word16leToReplyType 506
  = Right $ MultiZoneReplyType StateMultiZoneReply
word16leToReplyType x
  = Left $ "no case for " <> show x

-- | Documented and undocumented message types
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
