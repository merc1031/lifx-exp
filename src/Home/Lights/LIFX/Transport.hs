{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Home.Lights.LIFX.Transport where

import            Control.Concurrent.STM
import            Control.Monad                 ( when )
import            Control.Monad.Except
import            Control.Monad.Trans.Except
import            Data.Array.MArray             ( writeArray )
import            Data.Binary                   ( Binary (..) )
import            Data.Maybe                    ( isJust )
import            Data.Proxy
import            Network.Socket                ( Socket (..)
                                                , SockAddr (..)
                                                )
import            Network.Socket.ByteString
import qualified  Data.Binary                   as Bin
import qualified  Data.ByteString.Lazy          as BSL

import            Home.Lights.LIFX.Types


sendToLight
  :: ( Binary a
     )
  => SharedState
  -> Light
  -> Packet a
  -> IO ()
sendToLight ss@SharedState {} Light {..}
  = sendToDevice ss d
  where
    d@Device {..} = lDevice


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


deviceIdToTarget
  :: DeviceId
  -> Target
deviceIdToTarget (DeviceId m)
  = Target m


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
  :: Direction
  -> ProtocolHeader
mkProtocolHeader typ
  = ProtocolHeader 0 typ ()


mkRequestPacket
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
mkRequestPacket tag src tar ack res sequ typ pay
  =
  let
    f = mkFrame p tag src
    fa = mkFrameAddress tar ack res sequ
    ph = mkProtocolHeader (Request typ)
    -- Only make a refernce to `p` here to "tie the knot" since mkFrame needs the `p` size
    p = Packet f fa ph pay
  in
    p


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
  -> Direction
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


newDiscoveryPacket
  :: SharedState
  -> (SharedState -> Packet StateService -> SockAddr -> BSL.ByteString -> IO ())
  -> IO (Packet GetService)
newDiscoveryPacket ss@SharedState {..} runCb
  = do
  pp <- newPacket ss runCb
    $ \p@Packet {..} ->
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
newPacket' ss@SharedState {..} runCb
  = newPacket ss runCb id


newPacket
  :: forall a
   . ( MessageIdC a )
  => SharedState
  -> (SharedState -> Packet (StateReply a) -> SockAddr -> BSL.ByteString -> IO ())
  -> (Packet a -> Packet a)
  -> IO (a -> Packet a)
newPacket ss@SharedState {..} runCb modify
  = do
  nextSeq <- ssNextSeq
  setCallbackForSeq ss nextSeq $ CallbackWrap decodePacket runCb
  pure $ modify . mkRequestPacket
    SingleTagged
    ssUniqueSource
    (word64leToTarget 0)
    NoAckRequired
    NoResRequired
    nextSeq
    (msgTypP (Proxy :: Proxy a))
