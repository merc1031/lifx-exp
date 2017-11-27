{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib where


import Control.Monad (forM_, replicateM_, when, forever)
import Data.Binary (Binary (..), Get, Put)
import Data.Bits (zeroBits, Bits(..), bit, shiftR, shiftL)
import Data.Bool (bool)
import Data.Char (intToDigit)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Prim
import GHC.TypeLits
import Numeric
import Network.Socket ( Socket (..)
                      , SockAddr (..)
                      , tupleToHostAddress
                      , SocketOption (..)
                      , setSocketOption
                      , isSupportedSocketOption
                      , bind
                      , defaultProtocol
                      , aNY_PORT
                      , socket
                      , Family(AF_INET)
                      , SocketType(Datagram)
                      )
import Network.Socket.ByteString
import qualified Data.Binary as Bin
import qualified Data.Binary.Bits as Bits
import qualified Data.Binary.Put as BinP
import qualified Data.Binary.Get as BinG
import qualified Data.ByteString.Base16.Lazy as BSL16
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Info as NI
import qualified Data.HashMap.Strict as HM
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Arrow
import Data.Array.MArray ( writeArray, readArray, newListArray )




data Tagged
  = SingleTagged -- Encodes as 0, means FrameAddress `target` must be a MAC
  | AllTagged -- Encodes as 1, means FrameAddress must be 0 and will be sent to all lights
  deriving Show

data Addressable
  = NoFrameAddress
  | HasFrameAddress -- Encodes as 1, always, meaning a `target` field exists
  deriving Show

data AckRequired
  = NoAckRequired
  | AckRequired
  deriving Show

data ResRequired
  = NoResRequired
  | ResRequired
  deriving Show


data Reserved (d :: Symbol) a = Reserved

data Sized (n :: Nat) a

--data Promotable
--  = S
--  | U
--  | O
--
--
--type family (Flag a) :: Promotable where
--  Flag (Reserved d a)  = 'U
--  Flag (Sized n (Reserved d a))     = 'S
--  Flag a = 'O
--
--instance (Flag a ~ flag, Binary' flag a) => Binary a where
--  put = put' (Proxy :: Proxy flag)
--  get = get' (Proxy :: Proxy flag)
--
--class Binary' (flag :: Promotable) a where
--  put' :: Proxy flag -> a -> Put
--  get' :: Proxy flag -> Get a
--
--instance (Num a, Binary a, KnownNat n) => Binary' 'S (Reserved d (Sized n a)) where
--  put' _ _ = forM_ [1..(natVal' (proxy# :: Proxy# n))] $ \_ -> put @a 0
--  get' _ = pure Reserved
--
--instance (Num a, Binary a) => Binary' 'U (Reserved d a) where
--  put' _ _ = put @a 0
--  get' _ = pure Reserved
--
----instance Binary a => Binary' 'O a where
----  put' _ t = put @a t
----  get' _ = get
--
--instance Binary' 'O Frame where
--  put' _ f@(Frame {..}) = do
--    BinP.putWord16le fSize -- Discovered from size of rest
--    putFrame2ndByte f
--    BinP.putWord32le fSource
--    pure ()
--
--  get' _ = pure $ Frame undefined undefined undefined undefined undefined undefined
--
--putFrame2ndByte Frame {..} =
--    BinP.putWord16le $
--      (fProtocol `shiftL` 0) +
--      (bool 0 1 (addressableToBool fAddressable) `shiftL` 12) +
--      (bool 0 1 (taggedToBool fTagged) `shiftL` 13) +
--      (fromIntegral $ fOrigin `shiftL` 14)
--
--instance Binary' 'O FrameAddress where
--  put' _ f@(FrameAddress {..}) = do
--    BinP.putWord64le faTarget
--    put faReserved
--    BinP.putWord8 $
--      (bool 0 1 faResRequired `shiftL` 0) +
--      (bool 0 1 faAckRequired `shiftL` 1) +
--      (0 `shiftL` 2)
--    BinP.putWord8 faSequence
--    pure ()
--
--  get' _ = pure $ FrameAddress undefined undefined undefined undefined undefined undefined
--
--instance Binary' 'O UnusedMac where
--  put' _ _ = replicateM_ 6 $ BinP.putWord8 0
--
--  get' _ = pure $ UnusedMac ((), (), (), (), (), ())
--
--instance Binary' 'O ProtocolHeader where
--  put' _ p@(ProtocolHeader {..}) = do
--    BinP.putWord64le 0
--    BinP.putWord16le $ messageTypeToWord16 phType
--    BinP.putWord16le 0
--    pure ()
--
--  get' _ = pure $ ProtocolHeader undefined undefined undefined
--
--instance Binary' 'O SetColor where
--  put' _ sc@(SetColor {..}) = do
--    BinP.putWord8 0
--    put scColor
--    BinP.putWord32le scDuration
--    pure ()
--
--  get' _ = pure $ SetColor undefined undefined undefined
--
--instance Binary' 'O HSBK where
--  put' _ hsbk@(HSBK {..}) = do
--    BinP.putWord16le hsbkHue
--    BinP.putWord16le hsbkSaturation
--    BinP.putWord16le hsbkBrightness
--    BinP.putWord16le hsbkKelvin
--    pure ()
--
--  get' _ = pure $ HSBK undefined undefined undefined undefined

--instance (Num a, Binary a, KnownNat n) => Binary (Reserved d (Sized n a)) where
--  put _ = forM_ [1..(natVal' (proxy# :: Proxy# n))] $ \_ -> put @a 0
--  get = pure Reserved
--
--instance (Num a, Binary a) => Binary (Reserved d a) where
--  put _ = put @a 0
--  get = pure Reserved

taggedToBool SingleTagged = False
taggedToBool AllTagged = True

addressableToBool NoFrameAddress = False
addressableToBool HasFrameAddress = True

ackRequiredToBool NoAckRequired = False
ackRequiredToBool AckRequired = True

resRequiredToBool NoResRequired = False
resRequiredToBool ResRequired = True
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
  , faAckRequired :: AckRequired
  , faResRequired :: ResRequired
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

data GetService
  = GetService
  deriving Show

data StateService
  = StateService
  { ssService :: Word8
  , ssPort :: Word32
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
  -> AckRequired
  -> ResRequired
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
  -> AckRequired
  -> ResRequired
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

instance WithSize GetService where
  size _ = 0


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
      (bool 0 1 (resRequiredToBool faResRequired) `shiftL` 0) +
      (bool 0 1 (ackRequiredToBool faAckRequired) `shiftL` 1) +
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

instance Binary GetService where
  put _ = pure ()
  get = pure GetService

instance Binary StateService where
  put _ = pure ()
  get = do
    ssService <- BinG.getWord8
    ssPort <- BinG.getWord32le
    pure StateService {..}

binaryToReadable
  :: Binary a
  => a
  -> String
binaryToReadable
  = concatMap
  (\x -> showIntAtBase  16 intToDigit x "")
  . BSL.unpack
  . Bin.encode


broadcast :: Binary a => Socket -> SockAddr -> a -> IO ()
broadcast sock bcast a = do
  --let
  --  hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
  --addr:_ <- getAddrInfo (Just hints) (Just "255.255.255.255") (Just "56700")
  --sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

  let
    enc = Bin.encode a
  print $ BSL16.encode $ enc
  sendManyTo sock (BSL.toChunks enc) bcast
-- BinP.runPut $ BinP.putInt16le ((1 `shiftL` 13) + (1 `shiftL` 12) + (1024))

newtype Callback
  = Callback { runCallback :: BSL.ByteString -> IO () }

data State
  = State
  { sReplyCallbacks :: TArray Word8 Callback
  , sReceiveThread :: Async ()
  , sDiscoveryThread :: Async ()
  , sLights :: [Light]
  , sSocket :: Socket
  }

data Light
  = Light
  { lService :: Word8
  , lPort :: Word32
  }

receiveThread sock replyCallbacks = async $ forever $ do
  print "Recieving"
  threadDelay $ 1 * 1000000
  (bs, sa) <- recvFrom sock 1500
  print bs
  pure ()

discoveryThread sock bcast port replyCallbacks  = async $ forever $ do
  print "Sending"
  threadDelay $ 1 * 1000000
  broadcast sock bcast (mkPacket AllTagged 0 1234  NoAckRequired ResRequired 0 (DeviceMessageType GetServiceMessage) GetService)
  pure ()

mkState = do
  ifaces <- (HM.fromList . (fmap $ NI.name &&& id)) <$> NI.getNetworkInterfaces
  let NI.IPv4 hostAddr = case HM.lookup "eth0" ifaces of
        Just iface -> NI.ipv4 iface
        Nothing -> undefined
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet aNY_PORT hostAddr
  when (isSupportedSocketOption Broadcast) (setSocketOption sock Broadcast 1)
  let bcast = SockAddrInet (fromIntegral port) (tupleToHostAddress (255,255,255,255)) --0xffffffff -- 255.255.255.255
      port = 56700
  sReplyCallbacks <- atomically $ newListArray (0, 255) (map (const $ Callback $ const $ pure ()) [0..255])

  sReceiveThread <- receiveThread sock sReplyCallbacks
  sDiscoveryThread <- discoveryThread sock bcast port sReplyCallbacks

  pure $ State sReplyCallbacks sReceiveThread sDiscoveryThread [] sock

--  hostAddr <- ifaceAddr $ fmap T.unpack ifname
--  sock <- socket AF_INET Datagram defaultProtocol
--  bind sock $ SockAddrInet aNY_PORT hostAddr
--  when (isSupportedSocketOption Broadcast) (setSocketOption sock Broadcast 1)
--  hostPort <- socketPort sock
--  let port = 56700 `fromMaybe` mport
--      bcast = SockAddrInet (fromIntegral port) 0xffffffff -- 255.255.255.255
--      source = mkSource hostAddr (fromIntegral hostPort)
--  tmv <- newEmptyTMVarIO
--  thr <- forkFinally (dispatcher tmv) (\_ -> close sock)
--  wthr <- mkWeakThreadId thr
--  atomically $ do
--    st <- newState ifname source sock bcast wthr mlog
--    putTMVar tmv st
--    return st
--
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
