{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
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


import Control.Monad (forM_, replicateM_, when, forever, void)
import Data.Binary (Binary (..), Get, Put)
import Data.Bits (zeroBits, Bits(..), bit, shiftR, shiftL, testBit)
import Data.Bool (bool)
import Data.Monoid ((<>))
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
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.Control







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

boolToTagged False = SingleTagged
boolToTagged True = AllTagged

boolToAddressable False = NoFrameAddress
boolToAddressable True = HasFrameAddress

boolToAckRequired False = NoAckRequired
boolToAckRequired True = AckRequired

boolToResRequired False = NoResRequired
boolToResRequired True = ResRequired
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
  , fSource :: UniqueSource
  } deriving Show

-- Layout in bits:   64|48|6|1|1|8
--                   ui|ui|r|b|b|ui
--                      [6]
data FrameAddress
  = FrameAddress
  { faTarget :: Target
  , faReserved :: UnusedMac -- 0
  , faReserved2 :: ()
  , faAckRequired :: AckRequired
  , faResRequired :: ResRequired
  , faSequence :: Sequence
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
  | StateServiceMessage
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


word16ToMessageType 3 = Right $ DeviceMessageType StateServiceMessage
word16ToMessageType x = Left $ "no case for " <> show x
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
  -> UniqueSource
  -> Frame
mkFrame par tag src = Frame (size par) 0 tag HasFrameAddress 1024 src

mkFrameAddress
  :: Target
  -> AckRequired
  -> ResRequired
  -> Sequence
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
  -> UniqueSource
  -> Target
  -> AckRequired
  -> ResRequired
  -> Sequence
  -> MessageType
  -> a
  -> Packet a
mkPacket tag src tar ack res seq typ pay =
  let f = mkFrame p tag src
      fa = mkFrameAddress tar ack res seq
      ph = mkProtocolHeader typ
      p = Packet f fa ph pay
  in p

serviceUDP = 1

newtype UniqueSource
  = UniqueSource { unUniqueSource :: Word32 }
  deriving (Show, Eq)

newtype Target
  = Target { unTarget :: Word64 }
  deriving Show

newtype Sequence
  = Sequence { unSequence :: Word8 }
  deriving (Show, Eq)


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

instance Binary Target where
  put (Target t) = BinP.putWord64le t
  get = Target <$> BinG.getWord64le

instance Binary UniqueSource where
  put (UniqueSource t) = BinP.putWord32le t
  get = pure $ UniqueSource undefined

instance Binary Sequence where
  put (Sequence t) = BinP.putWord8 t
  get = Sequence <$> BinG.getWord8

instance Binary Frame where
  put f@Frame {..} = do
    BinP.putWord16le fSize -- Discovered from size of rest
    putFrame2ndByte f
    Bin.put fSource

  get = do
    fSize <- BinG.getWord16le
    frame2ndByte <- BinG.getWord16le
    let
      fProtocol = extract frame2ndByte 0 11
      fOrigin = extract frame2ndByte 14 2
      fAddressable = boolToAddressable $ testBit frame2ndByte 12
      fTagged = boolToTagged $ testBit frame2ndByte 13


    fSource <- UniqueSource <$> BinG.getWord32le
    pure Frame {..}

--Now your algorithm for cutting out from M to N becomes a two-step process: you shift the original value M bits to the right, and then perform a bit-wise AND with the mask of N-M ones.
extract :: (Integral a, Bits a, Integral b) => a -> Int -> Int -> b
extract x m n = fromIntegral field
  where field = (x `shiftR` m) .&. mask
        mask = bit w - 1
        w = n - m



putFrame2ndByte Frame {..} =
    BinP.putWord16le $
      (fProtocol `shiftL` 0) +
      (bool 0 1 (addressableToBool fAddressable) `shiftL` 12) +
      (bool 0 1 (taggedToBool fTagged) `shiftL` 13) +
      (fromIntegral $ fOrigin `shiftL` 14)

instance Binary FrameAddress where
  put f@FrameAddress {..} = do
    Bin.put faTarget
    Bin.put faReserved
    BinP.putWord8 $
      (bool 0 1 (resRequiredToBool faResRequired) `shiftL` 0) +
      (bool 0 1 (ackRequiredToBool faAckRequired) `shiftL` 1) +
      (0 `shiftL` 2)
    Bin.put faSequence

  get = do
    faTarget <- Bin.get
    faReserved <- Bin.get
    frameAddress15thByte <- BinG.getWord8
    let
      _faReserved2 = extract frameAddress15thByte 2 7
      faReserved2 = ()
      faResRequired = boolToResRequired $ testBit frameAddress15thByte 0
      faAckRequired = boolToAckRequired $ testBit frameAddress15thByte 1
    faSequence <- Bin.get
    pure FrameAddress {..}

instance Binary UnusedMac where
  put _ = replicateM_ 6 $ BinP.putWord8 0

  get = do
    void $ replicateM_ 6 $ BinG.getWord8
    pure $ UnusedMac ((), (), (), (), (), ())

instance Binary ProtocolHeader where
  put p@ProtocolHeader {..} = do
    BinP.putWord64le 0
    BinP.putWord16le $ messageTypeToWord16 phType
    BinP.putWord16le 0

  get = do
    _ <- BinG.getWord64le
    phType_ <- word16ToMessageType <$> BinG.getWord16le
    phType <- case phType_ of
      Left p -> fail $ show p
      Right p -> pure $ p

    _ <- BinG.getWord16le

    pure ProtocolHeader {phReserved = 0,phReserved2 = (),..}

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

instance Binary Header where
  put _ = pure ()
  get = do
    hFrame <- Bin.get
    hFrameAddress <- Bin.get
    hProtocolHeader <- Bin.get
    pure $ Header {..}

data Header
  = Header
  { hFrame :: Frame
  , hFrameAddress :: FrameAddress
  , hProtocolHeader :: ProtocolHeader
  } deriving Show

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

data AppState
  = AppState
  { asReplyCallbacks :: TArray Word8 Callback
  , asReceiveThread :: Async ()
  , asDiscoveryThread :: Async ()
  , asLights :: [Light]
  , asSocket :: Socket
  }

data SharedState
  = SharedState
  { ssReplyCallbacks :: TArray Word8 Callback
  , ssLights :: [Light]
  , ssSocket :: Socket
  , ssNextSeq :: IO Sequence
  }

data Light
  = Light
  { lService :: Word8
  , lPort :: Word32
  }

data Error = Error

decodePacket :: (MonadError Error m, Binary a) => BSL.ByteString -> (a -> m ()) -> m ()
decodePacket bs cont =
  case Bin.decodeOrFail bs of
    Left _ -> throwError Error
    Right (rem, cons, hdr) -> do
      let
        packetSize = fSize $ hFrame hdr
        packetSource = fSource $ hFrame hdr
      when (packetSize /= (fromIntegral $ BSL.length bs)) $ throwError Error
      when (packetSource /= uniqueSource) $ throwError Error
      case Bin.decodeOrFail rem of
        Left _ -> throwError Error
        Right (_, _, payload) -> cont payload

uniqueSource = UniqueSource 1234

receiveThread
  :: SharedState
  -> IO (Async ())
receiveThread SharedState {..} = async $ forever $ do
  threadDelay $ 1 * 100000
  (bs, sa) <- recvFrom ssSocket 1500
  print $ "Received 16" <> (show $ BSL16.encode $ BSL.fromStrict bs)
  runExceptT $ decodePacket (BSL.fromStrict bs) $ \StateService {..} -> do
    lift $ print $ show ssService
    lift $ print $ show ssPort
  pure ()

onStateService = do
    lift $ print $ show ssService
    lift $ print $ show ssPort

discoveryThread
  :: SharedState
  -> SockAddr
  -> IO (Async ())
discoveryThread ss@(SharedState {..}) bcast = async $ forever $ do
  threadDelay $ 3 * 1000000
  nextSeq <- ssNextSeq
  setCallbackForSeq ss nextSeq onStateService
  broadcast
    ssSocket
    bcast
    $ mkPacket
        AllTagged
        uniqueSource
        (Target 0)
        NoAckRequired
        ResRequired
        nextSeq
        (DeviceMessageType GetServiceMessage)
        GetService
  pure ()

setCallbackForSeq
  :: ( MonadError e m
     , Binary a
     )
  => SharedState
  -> Sequence
  -> (a -> SockAddr -> m ())
setCallbackForSeq SharedState {..} seq cont =
  writeArray ssReplyCallbacks (unSequence seq) cont

mkState :: IO AppState
mkState = do
  nSeq <- newTVarIO (Sequence 0)
  ssNextSeq <- pure $ atomically $ do
    val@(Sequence inner) <- readTVar nSeq
    writeTVar nSeq $! Sequence (inner + 1)
    pure val

  ifaces <- (HM.fromList . (fmap $ NI.name &&& id)) <$> NI.getNetworkInterfaces
  print $ "IFaces " <> show ifaces
  let net@(NI.IPv4 hostAddr) = case HM.lookup "eth0" ifaces of
        Just iface -> NI.ipv4 iface
        Nothing -> undefined

  print $ "Net Address " <> show net
  print $ "Host Address " <> show hostAddr
  ssSocket <- socket AF_INET Datagram defaultProtocol
  let bcast = SockAddrInet (fromIntegral port) (tupleToHostAddress (255,255,255,255)) --0xffffffff -- 255.255.255.255
      port = 56700
      addr = SockAddrInet (port + 1) 0

  print $ "Sock Addr " <> show addr
  when (isSupportedSocketOption Broadcast) (setSocketOption ssSocket Broadcast 1)
  bind ssSocket addr
  ssReplyCallbacks <- atomically $ newListArray (0, 255) (map (const $ Callback $ const $ pure ()) [0..255])

  let
    ssLights = []
    sharedState = SharedState {..}
  asReceiveThread <- receiveThread sharedState
  asDiscoveryThread <- discoveryThread sharedState bcast

  pure $ AppState ssReplyCallbacks asReceiveThread asDiscoveryThread ssLights ssSocket


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
