{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
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


data Reserved (d :: Symbol) a = Reserved

data Sized (n :: Nat) a


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
mkFrame par tag = Frame (size par) 0 tag HasFrameAddress 1024

mkFrameAddress
  :: Target
  -> AckRequired
  -> ResRequired
  -> Sequence
  -> FrameAddress
mkFrameAddress tar = FrameAddress tar (UnusedMac ((), (), (), (), (), ())) ()

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

instance WithSize StateService where
  size _ = 5

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
      fromIntegral (fOrigin `shiftL` 14)

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
    void $ replicateM_ 6 BinG.getWord8
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
      Right p -> pure p

    _ <- BinG.getWord16le

    pure ProtocolHeader {phReserved = 0,phReserved2 = (),..}

instance Binary SetColor where
  put sc@SetColor {..} = do
    BinP.putWord8 0
    put scColor
    BinP.putWord32le scDuration
    pure ()

  get = pure $ SetColor undefined undefined undefined

instance Binary HSBK where
  put hsbk@HSBK {..} = do
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
    when (ssService /= 1) $ fail $ "Not a StateService mismatched service: " <> show ssService

    ssPort <- BinG.getWord32le
    pure StateService {..}

instance Binary Header where
  put _ = pure ()
  get = do
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


broadcast :: Binary a => Socket -> SockAddr -> a -> IO ()
broadcast sock bcast a = do
  --let
  --  hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
  --addr:_ <- getAddrInfo (Just hints) (Just "255.255.255.255") (Just "56700")
  --sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

  let
    enc = Bin.encode a
  print $ BSL16.encode enc
  sendManyTo sock (BSL.toChunks enc) bcast
-- BinP.runPut $ BinP.putInt16le ((1 `shiftL` 13) + (1 `shiftL` 12) + (1024))

data Callback
  = forall a e m. (MonadError e m, Binary a, WithSize a) => Callback { runDecode :: BSL.ByteString -> m a, runCallback :: a -> SockAddr -> IO () }

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

--decodePacketThen :: (MonadError Error m, Binary a) => BSL.ByteString -> (a -> m ()) -> m ()
--decodePacketThen bs cont =
--  case Bin.decodeOrFail bs of
--    Left _ -> throwError Error
--    Right (rem, cons, hdr) -> do
--      let
--        packetSize = fSize $ hFrame hdr
--        packetSource = fSource $ hFrame hdr
--      when (packetSize /= (fromIntegral $ BSL.length bs)) $ throwError Error
--      when (packetSource /= uniqueSource) $ throwError Error
--      case Bin.decodeOrFail rem of
--        Left _ -> throwError Error
--        Right (_, _, payload) -> cont payload

--decodePacket :: forall m a. (MonadError Error m, Binary a, WithSize a) => BSL.ByteString -> m a
decodePacket :: (Binary a, WithSize a) => BSL.ByteString -> Except Error a
decodePacket bs =
  case Bin.decodeOrFail bs of
    Left _ -> throwE Error
    Right (rem, cons, hdr) -> do
      let
        packetSize = fSize $ hFrame hdr
        packetSource = fSource $ hFrame hdr
      when (packetSize /= fromIntegral (BSL.length bs)) $ throwError Error
      when (packetSource /= uniqueSource) $ throwError Error
      case Bin.decodeOrFail rem of
        Left _ -> throwE Error
        Right (_, _, payload) -> pure payload

uniqueSource = UniqueSource 1234

receiveThread
  :: SharedState
  -> IO (Async ())
receiveThread SharedState {..} = async $ forever $ do
  threadDelay $ 1 * 100000
  (bs, sa) <- recvFrom ssSocket 1500
  print $ "Received 16" <> show (BSL16.encode $ BSL.fromStrict bs)
  runExceptT $ decodePacketThen (BSL.fromStrict bs) $ \StateService {..} -> do
    lift $ print $ show ssService
    lift $ print $ show ssPort
  pure ()

onStateService StateService {..} sa = do
    print $ show ssService
    print $ show ssPort

discoveryThread
  :: SharedState
  -> SockAddr
  -> IO (Async ())
discoveryThread ss@SharedState {..} bcast = async $ forever $ do
  threadDelay $ 3 * 1000000
  nextSeq <- ssNextSeq
  runExceptT $ setCallbackForSeq ss nextSeq $ Callback decodePacket onStateService
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
     , MonadIO m
     )
  => SharedState
  -> Sequence
  -> Callback
  -> m ()
setCallbackForSeq SharedState {..} seq cont =
  liftIO $ atomically $ writeArray ssReplyCallbacks (unSequence seq) cont

mkState :: IO AppState
mkState = do
  nSeq <- newTVarIO (Sequence 0)
  ssNextSeq <- pure $ atomically $ do
    val@(Sequence inner) <- readTVar nSeq
    writeTVar nSeq $! Sequence (inner + 1)
    pure val

  ifaces <- (HM.fromList . fmap (NI.name &&& id)) <$> NI.getNetworkInterfaces
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
  ssReplyCallbacks <- atomically $ newArray_ (0, 255) --newListArray (0, 255) (map (const $ Callback decodePacket $ const $ const $ pure ()) [0..255])

  let
    ssLights = []
    sharedState = SharedState {..}
  asReceiveThread <- receiveThread sharedState
  asDiscoveryThread <- discoveryThread sharedState bcast

  pure $ AppState ssReplyCallbacks asReceiveThread asDiscoveryThread ssLights ssSocket


instance Binary a => Binary (Packet a) where
  put Packet {..} = do
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
