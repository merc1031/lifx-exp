{-# LANGUAGE RecordWildCards #-}
module Lib where


import Data.Binary (Binary (..))
--import qualified Data.Binary.Bits as Bits
import qualified Data.Binary as Bin
import qualified Data.Binary.Put as BinP
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Bits (zeroBits, Bits(..), bit, shiftR, shiftL)
import Numeric


-- Little endian
--
--
-- Layout in bits:   16|2|1|1|12|32
--                   ui|ui|b|b|ui|ui
data Frame
  = Frame
  { fSize :: Int16
  , fOrigin :: Int8 -- 0
  , fTagged :: Bool
  , fAddressable :: Bool -- 1
  , fProtocol :: Int16 -- 1024
  , fSource :: Int32
  }

-- Layout in bits:   64|48|6|1|1|8
--                   ui|ui|r|b|b|ui
--                      [6]
data FrameAddress
  = FrameAddress
  { faTarget :: Int64
  , faReserved :: (Int8, Int8, Int8, Int8, Int8, Int8) -- 0
  , faReserved2 :: ()
  , faAckRequired :: Bool
  , faResRequired :: Bool
  , faSequence :: Int8
  }

-- Layout in bits:   64|16|16
--                   ui|ui|r
data ProtocolHeader
  = ProtocolHeader
  { phReserved :: Int64
  , phType :: Int16
  , phReserved2 :: ()
  }

data Packet a
  = Packet
  { pFrame :: Frame
  , pFrameAddress :: FrameAddress
  , pProtocolHeader :: ProtocolHeader
  , pPayload :: a
  }


instance Binary Frame where
  put (Frame {..}) = do
    BinP.putInt16le fSize -- Discovered from size of rest
    BinP.putInt16le $ 1
    pure ()

  get = pure $ Frame undefined undefined undefined undefined undefined undefined


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
--instance Binary a => Binary (Packet a) where
--  put (Packet {..}) = do
--    put pFrame
--    put pFrameAddress
--    put pProtocolHeader
--    put pPayload
