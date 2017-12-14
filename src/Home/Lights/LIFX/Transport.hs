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
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Home.Lights.LIFX.Transport where

import            Control.Applicative           ( (<|>) )
import            Control.Arrow
import            Control.Concurrent
import            Control.Concurrent.Async
import            Control.Concurrent.STM
import            Control.Concurrent.STM.TQueue
import            Control.DeepSeq
import            Control.DeepSeq.Generics
import            Data.Void
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
import            Control.Monad.Trans.Maybe
import            Data.Array.MArray             ( writeArray
                                                , readArray
                                                , newArray_
                                                , newListArray
                                                )
import            Data.Binary                   ( Binary (..)
                                                , Get
                                                , Put
                                                )
import            Data.Binary.IEEE754
import            Data.Bits                     ( zeroBits
                                                , Bits(..)
                                                , FiniteBits(..)
                                                , bit
                                                , shiftR
                                                , shiftL
                                                , testBit
                                                )
import            Data.Bool                     ( bool )
import            Data.Char                     ( intToDigit
                                                , isPrint
                                                , toLower
                                                )
import            Data.Coerce
import            Data.Default
import            Data.Functor.Identity         ( Identity )
import            Data.Generics.Product
import            Data.Generics.Sum
import            Data.Hashable                 ( Hashable )
import            Data.Int                      ( Int8
                                                , Int16
                                                , Int32
                                                , Int64
                                                )
import            Data.Maybe                    ( isJust
                                                , fromJust
                                                )
import            Data.Monoid                   ( (<>) )
import            Data.Proxy
import            Data.Time.Clock
import            Data.Time.Clock.POSIX
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
import            System.Environment
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

import            Home.Lights.LIFX.Types


sendToLight
  :: ( Binary a
     )
  => SharedState
  -> Light
  -> Packet a
  -> IO ()
sendToLight ss@(SharedState {}) Light {..} packet
  = sendToDevice ss d packet
  where
    d@(Device {..}) = lDevice

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
