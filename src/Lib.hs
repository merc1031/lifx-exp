{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib where

import            Control.Arrow
import            Control.Concurrent
import            Control.Concurrent.Async
import            Control.Concurrent.STM
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
                                                , bit
                                                , shiftR
                                                , shiftL
                                                , testBit
                                                )
import            Data.Bool                     ( bool )
import            Data.Char                     ( intToDigit )
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
import            Numeric
import qualified  Data.Binary                   as Bin
import qualified  Data.Binary.Bits              as Bits
import qualified  Data.Binary.Get               as BinG
import qualified  Data.Binary.Put               as BinP
import qualified  Data.ByteString.Base16.Lazy   as BSL16
import qualified  Data.ByteString.Lazy          as BSL
import qualified  Data.HashMap.Strict           as HM
import qualified  Data.Text.Lazy                as TL
import qualified  Network.Info                  as NI


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


data Reserved (d :: Symbol) a
  = Reserved

data Sized (n :: Nat) a


taggedToBool SingleTagged
  = False
taggedToBool AllTagged
  = True

addressableToBool NoFrameAddress
  = False
addressableToBool HasFrameAddress
  = True

ackRequiredToBool NoAckRequired
  = False
ackRequiredToBool AckRequired
  = True

resRequiredToBool NoResRequired
  = False
resRequiredToBool ResRequired
  = True

boolToTagged False
  = SingleTagged
boolToTagged True
  = AllTagged

boolToAddressable False
  = NoFrameAddress
boolToAddressable True
  = HasFrameAddress

boolToAckRequired False
  = NoAckRequired
boolToAckRequired True
  = AckRequired

boolToResRequired False
  = NoResRequired
boolToResRequired True
  = ResRequired

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
  }
  deriving Show

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
  }
  deriving Show


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
  }
  deriving Show

data Packet a
  = Packet
  { pFrame :: Frame
  , pFrameAddress :: FrameAddress
  , pProtocolHeader :: ProtocolHeader
  , pPayload :: a
  }
  deriving Show

-- Layout in bits:   16|16|16|16
--                   ui|ui|ui|ui
data HSBK
  = HSBK
  { hsbkHue :: Word16 -- 0-65535
  , hsbkSaturation :: Word16 -- 0-65535
  , hsbkBrightness :: Word16 -- 0-65535
  , hsbkKelvin :: Word16 --2500-9000
  }
  deriving Show

data Get
  = Get

data State
  = State
  { sColor :: HSBK
  , sReserved :: Int16
  , sPower :: Word16
  , sLabel :: TL.Text
  , sReserved2 :: Word64
  }
  deriving Show

-- | 32 bytes
newtype Label
  = Label { unLabel :: TL.Text }
  deriving Show

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
  = GetMessage
  | SetColorMessage
  | StateMessage
  deriving Show

data MultiZoneMessage
  = SetColorZonesMessage
  deriving Show

messageTypeToWord16
 = \case
 DeviceMessageType dm ->
   deviceMessageTypeToWord16 dm
 LightMessageType lm ->
   lightMessageTypeToWord16 lm
 MultiZoneMessageType mzm ->
   multiZoneMessageTypeToWord16 mzm
 where
  deviceMessageTypeToWord16 = \case
    GetServiceMessage -> 2
  lightMessageTypeToWord16 = \case
    GetMessage -> 101
    SetColorMessage -> 102
    StateMessage -> 107
  multiZoneMessageTypeToWord16 = \case
    SetColorZonesMessage -> 501


word16ToMessageType 3
  = Right $ DeviceMessageType StateServiceMessage
word16ToMessageType x
  = Left $ "no case for " <> show x

-- 102: Layout in bits:   8|_|32
--                        ui|hsbk|ui
data SetColor
  = SetColor
  { scReserved :: ()
  , scColor :: HSBK
  , scDuration :: Word32 -- ms
  }
  deriving Show

data GetService
  = GetService
  deriving Show

data StateService
  = StateService
  { ssService :: Word8
  , ssPort :: Word32
  }
  deriving Show

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
  = FrameAddress tar (UnusedMac ((), (), (), (), (), ())) ()

mkProtocolHeader
  :: MessageType
  -> ProtocolHeader
mkProtocolHeader typ
  = ProtocolHeader 0 typ ()

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
mkPacket tag src tar ack res seq typ pay
  =
  let
    f = mkFrame p tag src
    fa = mkFrameAddress tar ack res seq
    ph = mkProtocolHeader typ
    p = Packet f fa ph pay
  in
    p

serviceUDP
  = 1

newtype UniqueSource
  = UniqueSource { unUniqueSource :: Word32 }
  deriving (Show, Eq)

newtype Target
  = Target { unTarget :: Mac {-Word64-} }
  deriving Show

newtype Sequence
  = Sequence { unSequence :: Word8 }
  deriving (Show, Eq)


class WithSize a where
  size :: a -> Word16

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
    = BinP.putWord64le $ targetToWord64 t
  get
    = word64ToTarget <$> BinG.getWord64le


targetToWord64
  :: Target
  -> Word64
targetToWord64 (Target (Mac t))
  =
  let
    (b1, b2, b3, b4, b5, b6) = t

    r8 = fromIntegral b6 `shiftL` 2
    r7 = fromIntegral b5 `shiftL` 3
    r6 = fromIntegral b4 `shiftL` 4
    r5 = fromIntegral b3 `shiftL` 5
    r4 = fromIntegral b2 `shiftL` 6
    r3 = fromIntegral b1 `shiftL` 7
  in
    r8 + r7 + r6 + r5 + r4 + r3

word64ToTarget
  :: Word64
  -> Target
word64ToTarget n
  =
  let
    (b1, _) = extract' r1 1 2
    (b2, r1) = extract' r2 1 2
    (b3, r2) = extract' r3 1 2
    (b4, r3) = extract' r4 1 2
    (b5, r4) = extract' r5 1 2
    (b6, r5) = extract' dropped 1 2
  in
    Target $ Mac (b1, b2, b3, b4, b5, b6)
  where
    dropped = n `shiftR` 2

instance Binary UniqueSource where
  put (UniqueSource t)
    = BinP.putWord32le t
  get
    = pure $ UniqueSource undefined

instance Binary Sequence where
  put (Sequence t)
    = BinP.putWord8 t
  get
    = Sequence <$> BinG.getWord8

instance Binary Frame where
  put f@Frame {..}
    = do
    BinP.putWord16le fSize -- Discovered from size of rest
    putFrame2ndByte f
    Bin.put fSource

  get
    = do
    fSize <- BinG.getWord16le
    frame2ndByte <- BinG.getWord16le
    let
      fProtocol = extract frame2ndByte 0 11
      fOrigin = extract frame2ndByte 14 2
      fAddressable = boolToAddressable $ testBit frame2ndByte 12
      fTagged = boolToTagged $ testBit frame2ndByte 13


    fSource <- UniqueSource <$> BinG.getWord32le
    pure Frame {..}

-- | Cutting out from M to N becomes a two-step process: you shift the original value M bits to the right, and then perform a bit-wise AND with the mask of N-M ones.
extract :: (Integral a, Bits a, Integral b) => a -> Int -> Int -> b
extract x m n
  = fst $ extract' x m n

extract' :: (Integral a, Bits a, Integral b) => a -> Int -> Int -> (b, a)
extract' x m n
  = (fromIntegral field, shifted)
  where
    field = shifted .&. mask
    shifted = x `shiftR` m
    mask = bit w - 1
    w = n - m


putFrame2ndByte Frame {..}
  = BinP.putWord16le $
  (fProtocol `shiftL` 0) +
  (bool 0 1 (addressableToBool fAddressable) `shiftL` 12) +
  (bool 0 1 (taggedToBool fTagged) `shiftL` 13) +
  fromIntegral (fOrigin `shiftL` 14)

instance Binary FrameAddress where
  put f@FrameAddress {..}
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
    pure $ UnusedMac ((), (), (), (), (), ())

instance Binary ProtocolHeader where
  put p@ProtocolHeader {..}
    = do
    BinP.putWord64le 0
    BinP.putWord16le $ messageTypeToWord16 phType
    BinP.putWord16le 0

  get
    = do
    _ <- BinG.getWord64le
    phType_ <- word16ToMessageType <$> BinG.getWord16le
    phType <- case phType_ of
      Left p -> fail $ show p
      Right p -> pure p

    _ <- BinG.getWord16le

    pure ProtocolHeader {phReserved = 0,phReserved2 = (),..}

instance Binary SetColor where
  put sc@SetColor {..}
    = do
    BinP.putWord8 0
    put scColor
    BinP.putWord32le scDuration
    pure ()

  get
    = pure $ SetColor undefined undefined undefined

instance Binary HSBK where
  put hsbk@HSBK {..}
    = do
    BinP.putWord16le hsbkHue
    BinP.putWord16le hsbkSaturation
    BinP.putWord16le hsbkBrightness
    BinP.putWord16le hsbkKelvin
    pure ()

  get
    = pure $ HSBK undefined undefined undefined undefined

instance Binary GetService where
  put _
    = pure ()
  get
    = pure GetService

instance Binary StateService where
  put _
    = pure ()
  get
    = do
    ssService <- BinG.getWord8
    when (ssService /= 1) $ fail $ "Not a StateService mismatched service: " <> show ssService

    ssPort <- BinG.getWord32le
    pure StateService {..}

instance Binary Header where
  put _
    = pure ()
  get
    = do
    hFrame <- Bin.get
    hFrameAddress <- Bin.get
    hProtocolHeader <- Bin.get
    pure Header {..}

data Header
  = Header
  { hFrame :: Frame
  , hFrameAddress :: FrameAddress
  , hProtocolHeader :: ProtocolHeader
  } deriving Show


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

data Callback
  = forall a. (Binary a, WithSize a) =>
  Callback
  { runDecode :: Header -> BSL.ByteString -> Except PayloadDecodeError (Packet a)
  , runCallback :: SharedState -> Packet a -> SockAddr -> IO ()
  }

data AppState
  = AppState
  { asReceiveThread :: Async ()
  , asDiscoveryThread :: Async ()
  }

data SharedState
  = SharedState
  { ssReplyCallbacks :: TArray Word8 Callback
  , ssDevices :: TVar (HM.HashMap DeviceId Device)
  , ssSocket :: Socket
  , ssNextSeq :: IO Sequence
  }

newtype Mac
  = Mac (Word8, Word8, Word8, Word8, Word8, Word8)
  deriving (Show, Eq, Hashable)

newtype DeviceId
  = DeviceId Mac
  deriving (Show, Eq, Hashable)

newtype DeviceAddress
  = DeviceAddress Word32
  deriving (Show, Eq)

data DeviceSocketAddress
  = DeviceSocketAddress PortNumber DeviceAddress
  deriving (Show, Eq)

data Device
  = Device
  { dAddr :: DeviceSocketAddress
  , dDeviceId :: DeviceId
  } deriving (Show, Eq)

data HeaderDecodeError
  = NotAHeader
  { hdeError :: String
  , hdeOrig :: BSL.ByteString
  , hdeRemaining :: BSL.ByteString
  , hdeOffset :: BinG.ByteOffset
  }
  | ImproperSourceInHeader
  { hdeHeader :: Header
  , hdeOrig :: BSL.ByteString
  , hdeRemaining :: BSL.ByteString
  , hdeOffset :: BinG.ByteOffset
  }
  | ImproperSizeInHeader
  { hdeHeader :: Header
  , hdeOrig :: BSL.ByteString
  , hdeRemaining :: BSL.ByteString
  , hdeOffset :: BinG.ByteOffset
  }
  deriving Show

data PayloadDecodeError
  = PayloadDecodeFailed
  { pdeHeader :: Header
  , pdeRemaining :: BSL.ByteString
  , pdeRemainingAfterFail :: BSL.ByteString
  , pdeOffsetAfterFail :: BinG.ByteOffset
  , pdeError :: String
  }
  deriving Show

decodeHeader
  :: BSL.ByteString
  -> Except HeaderDecodeError (Header, BSL.ByteString)
decodeHeader bs
  = case Bin.decodeOrFail bs of
  Left (str, offset, err) ->
    throwE $ NotAHeader err bs str offset
  Right (rem, cons, hdr) -> do
    let
      packetSize = fSize $ hFrame hdr
      packetSource = fSource $ hFrame hdr
    when (packetSize /= fromIntegral (BSL.length bs))
      $ throwError $ ImproperSizeInHeader hdr bs rem cons
    when (packetSource /= uniqueSource)
      $ throwError $ ImproperSourceInHeader hdr bs rem cons
    pure (hdr, rem)

decodePacket
  :: ( Binary a
     , WithSize a
     )
  => Header
  -> BSL.ByteString
  -> Except PayloadDecodeError (Packet a)
decodePacket hdr rem
  = case Bin.decodeOrFail rem of
  Left (str, offset, err) ->
    throwE $ PayloadDecodeFailed hdr rem str offset err
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
  threadDelay $ 1 * 100000
  (bs, sa) <- recvFrom ssSocket 1500
  let
    headerE = runExcept $ decodeHeader (BSL.fromStrict bs)

  forM_ headerE $ \(header, rest) -> async $ do
    let
      Sequence seq = faSequence $ hFrameAddress header
    cb <- atomically $ readArray ssReplyCallbacks seq
    case cb of
      Callback {..} -> do
        let
          payloadE = runExcept $ runDecode header rest
        case payloadE of
          Right payload ->
            runCallback ss payload sa
          Left e ->
            print $ show e
  pure ()

onStateService SharedState {..} Packet {..} sa
  = do
  let
    incomingDevice = Device (socketAddrToDeviceSocketAddr sa) (DeviceId $ unTarget $ faTarget pFrameAddress)
  atomically $ do
    devs <- readTVar ssDevices
    case dDeviceId incomingDevice `HM.lookup` devs of
      Just l -> todo
      Nothing ->  todo
  where
    StateService {..} = pPayload
    todo = pure ()

data FoundDevice
  = FoundDevice
  {
  }

socketAddrToDeviceSocketAddr
  :: SockAddr
  -> DeviceSocketAddress
socketAddrToDeviceSocketAddr (SockAddrInet pa ha)
  = DeviceSocketAddress pa (DeviceAddress ha)

discoveryThread
  :: SharedState
  -> SockAddr
  -> IO (Async ())
discoveryThread ss@SharedState {..} bcast
  = async $ forever $ do
  threadDelay $ 3 * 1000000
  nextSeq <- ssNextSeq
  runExceptT $ setCallbackForSeq ss nextSeq $ Callback decodePacket onStateService
  broadcast
    ssSocket
    bcast
    $ mkPacket
        AllTagged
        uniqueSource
        (word64ToTarget 0)
        NoAckRequired
        ResRequired
        nextSeq
        (DeviceMessageType GetServiceMessage)
        GetService
  pure ()

setCallbackForSeq
  :: ( MonadError e m
     , MonadIO m
     )
  => SharedState
  -> Sequence
  -> Callback
  -> m ()
setCallbackForSeq SharedState {..} seq cont
  = liftIO
  $ atomically
  $ writeArray ssReplyCallbacks (unSequence seq) cont

mkState
  :: IO AppState
mkState = do
  nSeq <- newTVarIO (Sequence 0)
  ssNextSeq <- pure $ atomically $ do
    val@(Sequence inner) <- readTVar nSeq
    writeTVar nSeq $! Sequence (inner + 1)
    pure val

  ifaces <- (HM.fromList . fmap (NI.name &&& id)) <$> NI.getNetworkInterfaces
  let
    net@(NI.IPv4 hostAddr) = case HM.lookup "eth0" ifaces of
      Just iface -> NI.ipv4 iface
      Nothing -> undefined

  ssSocket <- socket AF_INET Datagram defaultProtocol
  let
    bcast = SockAddrInet (fromIntegral port) (tupleToHostAddress (255,255,255,255))
    port = 56700
    addr = SockAddrInet (port + 1) 0

  when (isSupportedSocketOption Broadcast)
    (setSocketOption ssSocket Broadcast 1)
  bind ssSocket addr
  ssReplyCallbacks <- atomically $ newArray_ (0, 255)
  ssDevices <- newTVarIO mempty

  let
    sharedState = SharedState {..}
  asReceiveThread <- receiveThread sharedState
  asDiscoveryThread <- discoveryThread sharedState bcast

  pure $ AppState asReceiveThread asDiscoveryThread


instance Binary a => Binary (Packet a) where
  put Packet {..}
    = do
    put pFrame
    put pFrameAddress
    put pProtocolHeader
    put pPayload

  get
    = pure $ Packet undefined undefined undefined undefined

packetFromHeader
  :: ( Binary a
     , WithSize a
     )
  => Header
  -> a
  -> Packet a
packetFromHeader Header {..} payload
  = Packet hFrame hFrameAddress hProtocolHeader payload
