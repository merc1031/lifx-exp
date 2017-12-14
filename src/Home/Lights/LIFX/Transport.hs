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

import            Control.Monad                 ( when )
import            Control.Monad.Except
import            Control.Monad.Trans.Except
import            Data.Binary                   ( Binary (..) )
import            Data.Maybe                    ( isJust )
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
