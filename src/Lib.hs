{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Lib where


import Control.Monad (forM_, replicateM_)
import Data.Binary (Binary (..))
import GHC.TypeLits
import Data.Proxy
import GHC.Prim
import qualified Data.Binary.Bits as Bits
import qualified Data.Binary as Bin
import qualified Data.Binary.Put as BinP
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits (zeroBits, Bits(..), bit, shiftR, shiftL)
import Numeric
import Data.Char (intToDigit)
import Data.Bool (bool)
import qualified Data.ByteString.Base16.Lazy as BSL16



data Tagged
  = SingleTagged
  | AllTagged
  deriving Show

data Addressable
  = NoFrameAddress
  | HasFrameAddress
  deriving Show


data Reserved (d :: Symbol) a = Reserved

data Sized (n :: Nat) a

instance (Num a, Binary a, KnownNat n) => Binary (Reserved d (Sized n a)) where
  put _ = forM_ [1..(natVal' (proxy# :: Proxy# n))] $ \_ -> put @a 0
  get = pure Reserved

instance (Num a, Binary a) => Binary (Reserved d a) where
  put _ = put @a 0
  get = pure Reserved

taggedToBool SingleTagged = False
taggedToBool AllTagged = True

addressableToBool NoFrameAddress = False
addressableToBool HasFrameAddress = True
-- Little endian
--
--
-- Layout in bits:   16|2|1|1|12|32
--                   ui|ui|b|b|ui|ui
data Frame
  = Frame
  { fSize :: Word16
  , fOrigin :: Word8 -- 0
  , fTagged :: Tagged
  , fAddressable :: Addressable -- 1
  , fProtocol :: Word16 -- 1024
  , fSource :: Word32
  } deriving Show

-- Layout in bits:   64|48|6|1|1|8
--                   ui|ui|r|b|b|ui
--                      [6]
data FrameAddress
  = FrameAddress
  { faTarget :: Word64
  , faReserved :: UnusedMac -- 0
  , faReserved2 :: ()
  , faAckRequired :: Bool
  , faResRequired :: Bool
  , faSequence :: Word8
  } deriving Show


newtype UnusedMac
  = UnusedMac ((), (), (), (), (), ())
  deriving Show

-- Layout in bits:   64|16|16
--                   ui|ui|r
data ProtocolHeader
  = ProtocolHeader
  { phReserved :: Word64
  , phType :: MessageType
  , phReserved2 :: ()
  } deriving Show

data Packet a
  = Packet
  { pFrame :: Frame
  , pFrameAddress :: FrameAddress
  , pProtocolHeader :: ProtocolHeader
  , pPayload :: a
  } deriving Show

-- Layout in bits:   16|16|16|16
--                   ui|ui|ui|ui
data HSBK
  = HSBK
  { hsbkHue :: Word16 -- 0-65535
  , hsbkSaturation :: Word16 -- 0-65535
  , hsbkBrightness :: Word16 -- 0-65535
  , hsbkKelvin :: Word16 --2500-9000
  } deriving Show

data MessageType
  = DeviceMessageType DeviceMessage
  | LightMessageType LightMessage
  | MultiZoneMessageType MultiZoneMessage
  deriving Show

data DeviceMessage
  = GetServiceMessage
  deriving Show

data LightMessage
  = SetColorMessage
  deriving Show

data MultiZoneMessage
  = SetColorZonesMessage
  deriving Show

messageTypeToWord16
 = \case
 DeviceMessageType dm -> deviceMessageTypeToWord16 dm
 LightMessageType lm -> lightMessageTypeToWord16 lm
 MultiZoneMessageType mzm -> multiZoneMessageTypeToWord16 mzm
 where
  deviceMessageTypeToWord16 = \case
    GetServiceMessage -> 2
  lightMessageTypeToWord16 = \case
    SetColorMessage -> 102
  multiZoneMessageTypeToWord16 = \case
    SetColorZonesMessage -> 501

-- 102: Layout in bits:   8|_|32
--                        ui|hsbk|ui
data SetColor
  = SetColor
  { scReserved :: ()
  , scColor :: HSBK
  , scDuration :: Word32 -- ms
  } deriving Show

mkFrame
  :: WithSize a
  => Packet a
  -> Tagged
  -> Word32
  -> Frame
mkFrame par tag src = Frame (size par) 0 tag HasFrameAddress 1024 src

mkFrameAddress
  :: Word64
  -> Bool
  -> Bool
  -> Word8
  -> FrameAddress
mkFrameAddress tar ack res seq = FrameAddress tar (UnusedMac ((), (), (), (), (), ())) () ack res seq

mkProtocolHeader
  :: MessageType
  -> ProtocolHeader
mkProtocolHeader typ = ProtocolHeader 0 typ ()

mkPacket
  :: ( WithSize a
     , Binary a
     )
  => Tagged
  -> Word32
  -> Word64
  -> Bool
  -> Bool
  -> Word8
  -> MessageType
  -> a
  -> Packet a
mkPacket tag src tar ack res seq typ pay =
  let f = mkFrame p tag src
      fa = mkFrameAddress tar ack res seq
      ph = mkProtocolHeader typ
      p = Packet f fa ph pay
  in p

class WithSize a where
  size :: a -> Word16

instance WithSize a => WithSize (Packet a) where
  size Packet {..} = size pFrame + size pFrameAddress + size pProtocolHeader + size pPayload

instance WithSize Frame where
  size = const 8

instance WithSize FrameAddress where
  size = const 16

instance WithSize ProtocolHeader where
  size = const 12

instance WithSize SetColor where
  size SetColor {..} = 1 + size scColor + 4

instance WithSize HSBK where
  size = const 8


instance Binary Frame where
  put f@(Frame {..}) = do
    BinP.putWord16le fSize -- Discovered from size of rest
    putFrame2ndByte f
    BinP.putWord32le fSource
    pure ()

  get = pure $ Frame undefined undefined undefined undefined undefined undefined

putFrame2ndByte Frame {..} =
    BinP.putWord16le $
      (fProtocol `shiftL` 0) +
      (bool 0 1 (addressableToBool fAddressable) `shiftL` 12) +
      (bool 0 1 (taggedToBool fTagged) `shiftL` 13) +
      (fromIntegral $ fOrigin `shiftL` 14)

instance Binary FrameAddress where
  put f@(FrameAddress {..}) = do
    BinP.putWord64le faTarget
    put faReserved
    BinP.putWord8 $
      (bool 0 1 faResRequired `shiftL` 0) +
      (bool 0 1 faAckRequired `shiftL` 1) +
      (0 `shiftL` 2)
    BinP.putWord8 faSequence
    pure ()

  get = pure $ FrameAddress undefined undefined undefined undefined undefined undefined

instance Binary UnusedMac where
  put _ = replicateM_ 6 $ BinP.putWord8 0

  get = pure $ UnusedMac ((), (), (), (), (), ())

instance Binary ProtocolHeader where
  put p@(ProtocolHeader {..}) = do
    BinP.putWord64le 0
    BinP.putWord16le $ messageTypeToWord16 phType
    BinP.putWord16le 0
    pure ()

  get = pure $ ProtocolHeader undefined undefined undefined

instance Binary SetColor where
  put sc@(SetColor {..}) = do
    BinP.putWord8 0
    put scColor
    BinP.putWord32le scDuration
    pure ()

  get = pure $ SetColor undefined undefined undefined

instance Binary HSBK where
  put hsbk@(HSBK {..}) = do
    BinP.putWord16le hsbkHue
    BinP.putWord16le hsbkSaturation
    BinP.putWord16le hsbkBrightness
    BinP.putWord16le hsbkKelvin
    pure ()

  get = pure $ HSBK undefined undefined undefined undefined

binaryToReadable
  :: Binary a
  => a
  -> String
binaryToReadable
  = concatMap
  (\x -> showIntAtBase  16 intToDigit x "")
  . BSL.unpack
  . Bin.encode

-- BinP.runPut $ BinP.putInt16le ((1 `shiftL` 13) + (1 `shiftL` 12) + (1024))


--    Bits.runBitPut $ do
--      Bits.putWord8 2 fOrigin
--      Bits.putBool fTagged
--      Bits.putBool fAddressable
--      Bits.putInt16le 12 1024
--
--instance Binary FrameAddress where
--  put (FrameAddress {..}) = do
--
--instance Binary ProtocolHeader where
--  put (ProtocolHeader {..}) = do
--
instance Binary a => Binary (Packet a) where
  put (Packet {..}) = do
    put pFrame
    put pFrameAddress
    put pProtocolHeader
    put pPayload

  get = pure $ Packet undefined undefined undefined undefined


-- Example
--
-- HEX: 31 00 00 34 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 66 00 00 00 00 55 55 FF FF FF FF AC 0D 00 04 00 00
-- set all to green
--
-- Frame 49 0 True True 1024 0
-- FrameAddress 0 (0,0,0,0,0,0) () False False 0
-- ProtocolHeader 0 102 ()
-- Payload
--    SetColor 0 (HSBK 21845 65535 65535 3500) 1024
