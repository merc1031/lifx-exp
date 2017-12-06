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

import            Control.Arrow
import            Control.Concurrent
import            Control.Concurrent.Async
import            Control.Concurrent.STM
import            Control.Concurrent.STM.TQueue
import            Control.DeepSeq
import            Control.DeepSeq.Generics
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
import            Data.Array.MArray             ( writeArray
                                                , readArray
                                                , newArray_
                                                , newListArray
                                                )
import            Data.Binary                   ( Binary (..)
                                                , Get
                                                , Put
                                                )
import            Data.Bits                     ( zeroBits
                                                , Bits(..)
                                                , FiniteBits(..)
                                                , bit
                                                , shiftR
                                                , shiftL
                                                , testBit
                                                )
import            Data.Bool                     ( bool )
import            Data.Char                     ( intToDigit )
import            Data.Coerce
import            Data.Functor.Identity         ( Identity )
import            Data.Hashable                 ( Hashable )
import            Data.Int                      ( Int8
                                                , Int16
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


newtype Word16le
  = Word16le { unWord16le :: Word16 }
  deriving newtype (Num, Real, Enum, Integral, Show, Read, Eq, Ord, Bits, NFData)

newtype Word32le
  = Word32le { unWord32le :: Word32 }
  deriving newtype (Num, Real, Enum, Integral, Show, Read, Eq, Ord, Bits, NFData)

newtype Word64le
  = Word64le { unWord64le :: Word64 }
  deriving newtype (Num, Real, Enum, Integral, Show, Read, Eq, Ord, Bits, NFData)

--deriving newtype instance Arbitrary Word64le

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

data DeviceIdentifier
  = IdMac
  | IdIp
  | IdName
  deriving (Show, Eq)

listCached
  :: SharedState
  -> IO [Light]
listCached SharedState {..}
  = HM.elems <$> (atomically $ readTVar ssDevices)

-- Layout in bits:   16|2|1|1|12|32
--                   ui|ui|b|b|ui|ui
data Frame
  = Frame
  { fSize :: !Word16le
  , fOrigin :: !Word8 -- 0
  , fTagged :: !Tagged
  , fAddressable :: !Addressable -- 1
  , fProtocol :: !Word16le -- 1024
  , fSource :: !UniqueSource
  }
  deriving (Show, Eq, Generic)

instance NFData Frame where
  rnf
    = genericRnfV1


-- Layout in bits:   64|48|6|1|1|8
--                   ui|ui|r|b|b|ui
--                      [6]
data FrameAddress
  = FrameAddress
  { faTarget :: !Target
  , faReserved :: !UnusedMac -- 0
  , faReserved2 :: !()
  , faAckRequired :: !AckRequired
  , faResRequired :: !ResRequired
  , faSequence :: !Sequence
  }
  deriving (Show, Eq, Generic)

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
  { phReserved :: !Word64le
  , phType :: !Direction
  , phReserved2 :: !()
  }
  deriving (Show, Eq, Generic)

instance NFData ProtocolHeader where
  rnf
    = genericRnfV1


data Packet a
  = Packet
  { pFrame :: Frame
  , pFrameAddress :: FrameAddress
  , pProtocolHeader :: ProtocolHeader
  , pPayload :: a
  }
  deriving (Show, Eq)

-- Layout in bits:   16|16|16|16
--                   ui|ui|ui|ui
data HSBK
  = HSBK
  { hsbkHue :: !Word16le -- 0-65535
  , hsbkSaturation :: !Word16le -- 0-65535
  , hsbkBrightness :: !Word16le -- 0-65535
  , hsbkKelvin :: !Word16le --2500-9000
  }
  deriving (Show, Eq)

-- | 32 bytes
newtype Label
  = Label { unLabel :: TL.Text }
  deriving (Show, Eq)

instance Binary Label where
  put
    = Bin.put . unLabel
  get
    = (Label . TLE.decodeUtf8) <$> BinG.getLazyByteString 32

data Direction
  = Request MessageType
  | Reply ReplyType
  deriving (Show, Eq, Generic)

instance NFData Direction where
  rnf
    = genericRnfV1


data MessageType
  = DeviceMessageType DeviceMessage
  | LightMessageType LightMessage
  | MultiZoneMessageType MultiZoneMessage
  deriving (Show, Eq, Generic)

instance NFData MessageType where
  rnf
    = genericRnfV1


data ReplyType
  = DeviceReplyType DeviceReply
  | LightReplyType LightReply
  | MultiZoneReplyType MultiZoneReply
  deriving (Show, Eq, Generic)

instance NFData ReplyType where
  rnf
    = genericRnfV1


data DeviceMessage
  = GetServiceMessage
  -- HostInfo
  -- HostFirmware
  -- WifiInfo
  -- WifiFirmware
  | GetPowerMessage
  | SetPowerMessage
  | GetLabelMessage
  | SetLabelMessage
  -- Version
  | GetInfoMessage
  | GetLocationMessage
  | SetLocationMessage
  | GetGroupMessage
  | SetGroupMessage
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
  -- HostInfo
  -- HostFirmware
  -- WifiInfo
  -- WifiFirmware
  | StatePowerReply
  | StateLabelReply
  -- Version
  | StateInfoReply
  | AcknowledgementReply
  | StateLocationReply
  | StateGroupReply
  | EchoReply
  deriving (Show, Eq, Generic)

instance NFData DeviceReply where
  rnf
    = genericRnfV1

data LightReply
  = StateReply
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
    GetPowerMessage -> 20
    SetPowerMessage -> 21
    GetLabelMessage -> 23
    SetLabelMessage -> 24
    GetInfoMessage -> 34
    GetLocationMessage -> 48
    SetLocationMessage -> 49
    GetGroupMessage -> 51
    SetGroupMessage -> 52
    EchoMessage -> 58
  lightMessageTypeToWord16le = \case
    GetLightMessage -> 101
    SetColorMessage -> 102
    SetWaveformMessage -> 103
    SetWaveformOptionalMessage -> 119
    GetLightPowerMessage -> 116
    SetLightPowerMessage -> 117
    GetInfraredMessage -> 121
    SetInfraredMessage -> 122
  multiZoneMessageTypeToWord16le = \case
    SetColorZonesMessage -> 501
    GetColorZonesMessage -> 502

-- 13 15 17 19 are unused  33
word16leToReplyType
  :: Word16le
  -> Either String ReplyType
word16leToReplyType 3
  = Right $ DeviceReplyType StateServiceReply
word16leToReplyType 22
  = Right $ DeviceReplyType StatePowerReply
word16leToReplyType 25
  = Right $ DeviceReplyType StateLabelReply
word16leToReplyType 35
  = Right $ DeviceReplyType StateInfoReply
word16leToReplyType 45
  = Right $ DeviceReplyType AcknowledgementReply
word16leToReplyType 50
  = Right $ DeviceReplyType StateLocationReply
word16leToReplyType 53
  = Right $ DeviceReplyType StateGroupReply
word16leToReplyType 59
  = Right $ DeviceReplyType EchoReply

word16leToReplyType 107
  = Right $ LightReplyType StateReply
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

class MessageId a where
  type StateReply a = s | s -> a

  msgCons :: a

  msgId :: a -> Word16le
  msgId _ = msgIdP (Proxy :: Proxy a)

  msgIdP :: Proxy a -> Word16le
  msgIdP _ = msgId @a undefined


  msgTyp :: a -> MessageType
  msgTyp _ = msgTypP (Proxy :: Proxy a)

  msgTypP :: Proxy a -> MessageType
  msgTypP _ = msgTyp @a undefined


newtype ByteId16
  = ByteId16 { unByte16 :: [Word8] }
  deriving (Show, Eq)

instance Binary ByteId16 where
  put
    = Bin.put . unByte16
  get
    = do
    unByte16 <- sequence $ replicate 16 $ BinG.getWord8
    pure $ ByteId16 {..}

newtype LocationId
  = LocationId { unLocationId :: ByteId16 }
  deriving (Show, Eq)
  deriving newtype Binary

newtype GroupId
  = GroupId { unGroupId :: ByteId16 }
  deriving (Show, Eq)
  deriving newtype Binary


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
  msgCons = GetLocation
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
  { stlLocation :: LocationId
  , stlLabel :: Label
  , stlUpdatedAt :: LifxUTC
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
    pure $ StateLocation {..}

instance WithSize StateLocation where
  size
    = const 392

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
  msgCons = GetLightPower
  msgId = const 116
  msgTyp = const $ LightMessageType GetLightPowerMessage

data StateLightPower
  = StateLightPower
  { stlpLevel :: LightPower
  , stlpDuration :: Word32le
  }
  deriving Show

instance Binary StateLightPower where
  put StateLightPower {..}
    = do
    Bin.put stlpLevel
    Bin.put stlpDuration

  get
    = do
    stlpLevel <- Bin.get
    stlpDuration <- Bin.get
    pure $ StateLightPower {..}

instance WithSize StateLightPower where
  size
    = const 48

instance WithSize GetLightPower where
  size
    = const 0

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
  msgCons = GetLight
  msgId = const 101
  msgTyp = const $ LightMessageType GetLightMessage

data StateLight
  = StateLight
  { stliColor :: HSBK
  , stliReserved :: Int16
  , stliPower :: LightPower
  , stliLabel :: Label
  , stliReserved2 :: Word64le
  }
  deriving Show

newtype LightPower
  = LightPower { unLightPower :: Word16le }
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
    pure $ StateLight {..}

instance WithSize StateLight where
  size
    = const $ size (undefined :: HSBK) + 42

instance WithSize GetLight where
  size
    = const 0


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
  msgCons = GetLabel
  msgId = const 48
  msgTyp = const $ DeviceMessageType GetLabelMessage

newtype StateLabel
  = StateLabel
  { stlaLabel :: Label
  }
  deriving Show

instance Binary StateLabel where
  put
    = Bin.put . stlaLabel
  get
    = do
    stlaLabel <- Bin.get
    pure $ StateLabel {..}

instance WithSize StateLabel where
  size
    = const 256

instance WithSize GetLabel where
  size
    = const 0

data GetInfo
  = GetInfo
  deriving Show

instance Binary GetInfo where
  put
    = const $ pure ()
  get
    = pure GetInfo

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
  msgCons = GetGroup
  msgId = const 51
  msgTyp = const $ DeviceMessageType GetGroupMessage

data StateGroup
  = StateGroup
  { stgGroup :: GroupId
  , stgLabel :: Label
  , stgUpdatedAt :: LifxUTC
  }
  deriving Show

instance Binary StateGroup where
  put StateGroup {..} = do
    Bin.put stgGroup
    Bin.put stgLabel
    Bin.put stgUpdatedAt
  get = do
    stgGroup <- Bin.get
    stgLabel <- Bin.get
    stgUpdatedAt <- Bin.get
    pure $ StateGroup {..}

instance WithSize StateGroup where
  size
    = const 392

instance WithSize GetGroup where
  size
    = const 0

data SetPower
  = SetPower
  { spLevel :: !Word16le }
  deriving Show

instance Binary SetPower where
  put
    = Bin.put . spLevel
  get
    = SetPower <$> Bin.get

data StatePower
  = StatePower
  { stpLevel :: !Word16le }
  deriving Show

instance Binary StatePower where
  put
    = Bin.put . stpLevel
  get
    = StatePower <$> Bin.get

data SetLabel
  = SetLabel
  { selLabel :: !Label }
  deriving Show

instance Binary SetLabel where
  put
    = Bin.put . selLabel
  get
    = SetLabel <$> Bin.get

data State
  = State
  { sColor :: !HSBK
  , sReserved :: !Int16le
  , sPower :: !Word16le
  , sLabel :: !Label
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

-- 102: Layout in bits:   8|_|32
--                        ui|hsbk|ui
data SetColor
  = SetColor
  { scReserved :: !()
  , scColor :: !HSBK
  , scDuration :: !Word32le -- ms
  }
  deriving Show

data GetService
  = GetService
  deriving (Show, Eq)

data StateService
  = StateService
  { ssService :: !Word8
  , ssPort :: !Word32le
  }
  deriving Show

instance MessageId GetService where
  type StateReply GetService = StateService
  msgCons = GetService
  msgId = const 2
  msgTyp = const $ DeviceMessageType GetServiceMessage

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
    p = Packet f fa ph pay
  in
    p

serviceUDP
  :: Word8
serviceUDP
  = 1

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

instance WithSize SetColor where
  size SetColor {..}
    = 1 + size scColor + 4

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
  = (fromIntegral field, shifted)
  where
    field = shifted .&. bmask
    shifted = x `shiftR` m
    bmask = bit w - 1
    w = n - m


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
    = case phType of
    Request phTypeR -> do
      Bin.put @Word16le 0
      Bin.put $ messageTypeToWord16le phTypeR
      Bin.put @Word64le 0
    x -> fail $ "Attempting to encode a reply type" <> show x

  get
    = do
    _ <- Bin.get @Word64le
    phType_ <- word16leToReplyType <$> Bin.get
    phType <- case phType_ of
      Left p -> fail $ show p
      Right p -> pure $ Reply p --TODO FIxme

    _ <- Bin.get @Word16le

    pure ProtocolHeader {phReserved = 0,phReserved2 = (),..}

instance Binary SetColor where
  put _sc@SetColor {..}
    = do
    BinP.putWord8 0
    Bin.put scColor
    Bin.put scDuration

  get
    = do
    scReserved <- () <$ BinG.getWord8
    scColor <- Bin.get
    scDuration <- Bin.get
    pure $ SetColor {..}

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
    pure $ HSBK {..}

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
data Header
  = Header
  { hFrame :: !Frame
  , hFrameAddress :: !FrameAddress
  , hProtocolHeader :: !ProtocolHeader
  }
  deriving (Show, Generic)

instance NFData Header where
  rnf
    = genericRnfV1



broadcast
  :: Binary a
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

data CallbackWrap
  = forall a. (Show a, Binary a, WithSize a) =>
  CallbackWrap
  { runDecode :: !(Header -> BSL.ByteString -> Except PayloadDecodeError (Packet a))
  , runCallback :: !(Callback a)
  }

type Callback a
  = (SharedState -> Packet a -> SockAddr -> BSL.ByteString -> IO ())

instance Show CallbackWrap where
  show
    = const "Callback..."

data AppState
  = AppState
  { asSharedState :: !SharedState
  , asReceiveThread :: !(Async ())
  , asDiscoveryThread :: !(Async ())
  }

data SharedState
  = SharedState
  { ssReplyCallbacks :: !(TArray Word8 CallbackWrap)
  , ssDevices :: !(TVar (HM.HashMap DeviceId Light))
  , ssSocket :: !Socket
  , ssNextSeq :: !(IO Sequence)
  }

data Light
  = Light
  { lDevice :: Device
  , lGroup :: Maybe GroupId
  , lLocation :: Maybe LocationId
  , lLabel :: Maybe Label
  , lColor :: Maybe HSBK
  , lPower :: Maybe LightPower
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

newtype DeviceId
  = DeviceId (Mac Word8)
  deriving (Eq, Hashable)

instance Show DeviceId where
  show (DeviceId t)
    = printMac t

deviceIdToTarget
  :: DeviceId
  -> Target
deviceIdToTarget (DeviceId m)
  = Target m

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
  { dAddr :: !DeviceSocketAddress
  , dDeviceId :: !DeviceId
  }
  deriving (Show, Eq)

data HeaderDecodeError
  = NotAHeader
  { hdeError :: !String
  , hdeOrig :: !BSL.ByteString
  , hdeRemaining :: !BSL.ByteString
  , hdeOffset :: !BinG.ByteOffset
  }
  | ImproperSourceInHeader
  { hdeHeader :: !Header
  , hdeOrig :: !BSL.ByteString
  , hdeRemaining :: !BSL.ByteString
  , hdeOffset :: !BinG.ByteOffset
  }
  | ImproperSizeInHeader
  { hdeHeader :: !Header
  , hdeOrig :: !BSL.ByteString
  , hdeRemaining :: !BSL.ByteString
  , hdeOffset :: !BinG.ByteOffset
  }
  deriving (Show, Generic)

instance Exception HeaderDecodeError

instance NFData HeaderDecodeError where
  rnf
    = genericRnfV1

data PayloadDecodeError
  = PayloadDecodeFailed
  { pdeHeader :: !Header
  , pdeRemaining :: !BSL.ByteString
  , pdeRemainingAfterFail :: !BSL.ByteString
  , pdeOffsetAfterFail :: !BinG.ByteOffset
  , pdeError :: !String
  }
  deriving Show


decodeHeader
  :: BSL.ByteString
  -> Except HeaderDecodeError (Header, BSL.ByteString)
decodeHeader bs
  = case Bin.decodeOrFail bs of
  Left (str, offset, err) ->
    throwE $ NotAHeader err bs str offset
  Right (rema, cons, hdr) -> do
    let
      packetSize = fSize $ hFrame hdr
      packetSource = fSource $ hFrame hdr
    when (packetSize /= fromIntegral (BSL.length bs))
      $ throwE $ ImproperSizeInHeader hdr bs rema cons
    when (packetSource /= uniqueSource)
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
  logStr "About to recv"
  (!bs, sa) <- recvFrom ssSocket 1500
  print $  "Recvd: " <> show bs
  let
    bsl = BSL.fromStrict bs
    headerE = runExcept $ decodeHeader bsl

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
  --logStr $ "Got State Service " <> show pPayload <> " " <> show sa <> " " <> show pFrameAddress <> (show $ BSL16.encode orig)
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
            pure $ map (flip uncurry (ss, incomingDevice)) [getLocation', getGroup', getLabel', getLightPower', getLight']
          else pure []
        Nothing -> do
          writeTVar ssDevices $ HM.insert (dDeviceId incomingDevice) (Light incomingDevice Nothing Nothing Nothing Nothing Nothing) devs
          pure $ map (flip uncurry (ss, incomingDevice)) [getLocation', getGroup', getLabel', getLightPower', getLight']

    sequence_ moreInfo
  where
    StateService {..} = pPayload

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
    bytes = Bin.encode packet

updateLocation
  :: SharedState
  -> Device
  -> StateLocation
  -> IO ()
updateLocation SharedState {..} Device {..} StateLocation {..}
  = do
  pure ()


getLocation'
  :: SharedState
  -> Device
  -> IO ()
getLocation' ss d
  = do
  outerGet ss d $ \_ p@(Packet {..}) _ _ -> do
    let
      StateLocation {..} = pPayload
    print $ show p

getGroup'
  :: SharedState
  -> Device
  -> IO ()
getGroup' ss d
  = outerGet ss d $ \_ p@(Packet {..}) _ _ -> do
  let
    StateGroup {..} = pPayload
  print $ show p

getLabel'
  :: SharedState
  -> Device
  -> IO ()
getLabel' ss d
  = outerGet ss d $ \_ p@(Packet {..}) _ _ -> do
  let
    StateLabel {..} = pPayload
  print $ show p

getLightPower'
  :: SharedState
  -> Device
  -> IO ()
getLightPower' ss d
  = outerGet ss d $ \_ p@(Packet {..}) _ _ -> do
  let
    StateLightPower {..} = pPayload
  print $ show p

getLight'
  :: SharedState
  -> Device
  -> IO ()
getLight' ss d
  = outerGet ss d $ \_ p@(Packet {..}) _ _ -> do
  let
    StateLight {..} = pPayload
  print $ show p

outerGet
  :: forall get
   . MessageIdC get
  => SharedState
  -> Device
  -> (SharedState -> Packet (StateReply get) -> SockAddr -> BSL.ByteString -> IO ())
  -> IO ()
outerGet ss d cb
  = do
  p <- newPacket ss cb
  let
    fp = p msgCons
    np = fp { pFrameAddress = (pFrameAddress fp) { faTarget = deviceIdToTarget $ dDeviceId d} }
  sendToDevice ss d np

logStr
  :: String
  -> IO ()
logStr
  = print

data FoundDevice
  = FoundDevice
  {
  }

type MessageIdC c
  = ( WithSize c
    , MessageId c
    , Binary c
    , Show c
    , Show (StateReply c)
    , Binary (StateReply c)
    , WithSize (StateReply c)
    )

newPacket
  :: forall a
   . ( MessageIdC a )
  => SharedState
  -> (SharedState -> Packet (StateReply a) -> SockAddr -> BSL.ByteString -> IO ())
  -> IO (a -> Packet a)
newPacket ss@(SharedState {..}) runCb
  = do
  nextSeq <- ssNextSeq
  setCallbackForSeq ss nextSeq $ CallbackWrap decodePacket runCb
  pure $ mkPacket
    AllTagged
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
  gsp <- newPacket ss onStateService
  broadcast
    ssSocket
    bcast
    $ gsp GetService
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

mkState
  :: IO AppState
mkState
  = do
  print =<< getNumCapabilities
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
    sharedState = SharedState {..}
  asReceiveThread <- receiveThread sharedState
  asDiscoveryThread <- discoveryThread sharedState bcast

  pure $ AppState sharedState asReceiveThread asDiscoveryThread


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
    pure $ Packet {..}

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
