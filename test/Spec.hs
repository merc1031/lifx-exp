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
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import            Control.Monad                   ( forM_ )
import            Data.Binary
import            Data.Bits
import            Data.Proxy
import            Data.Semigroup
import            Generic.Random
import            GHC.Generics
import            Test.Hspec
import            Test.QuickCheck
import qualified  Data.Binary                     as Bin
import qualified  Data.ByteString.Base16.Lazy     as BSL16

import            Lib
import            Home.Lights.LIFX.Types

instance Arbitrary Direction where
  arbitrary
    = oneof
    [ Request <$> arbitrary
    , Reply <$> arbitrary
    ]

instance Arbitrary MessageType where
  arbitrary = genericArbitraryU

instance Arbitrary ReplyType where
  arbitrary = genericArbitraryU

instance Arbitrary DeviceMessage where
  arbitrary = genericArbitraryU

instance Arbitrary LightMessage where
  arbitrary = genericArbitraryU

instance Arbitrary MultiZoneMessage where
  arbitrary = genericArbitraryU

instance Arbitrary DeviceReply where
  arbitrary = genericArbitraryU

instance Arbitrary LightReply where
  arbitrary = genericArbitraryU

instance Arbitrary MultiZoneReply where
  arbitrary = genericArbitraryU

deriving newtype instance Arbitrary Word16le
deriving newtype instance Arbitrary Word32le
deriving newtype instance Arbitrary Word64le

deriving newtype instance Arbitrary Int16le
deriving newtype instance Arbitrary Int32le
deriving newtype instance Arbitrary Int64le

deriving newtype instance Arbitrary Target

deriving newtype instance Arbitrary a => Arbitrary (Mac a)

instance Arbitrary GetService where
  arbitrary = pure $ GetService

instance WithSize Int where
  size = Word16le . fromIntegral . finiteBitSize

instance Arbitrary Sequence where
  arbitrary
    = Sequence <$> arbitrary

instance Arbitrary UniqueSource where
  arbitrary
    = UniqueSource <$> arbitrary

instance Arbitrary UnusedMac where
  arbitrary
    = pure $ UnusedMac $ Mac ((), (), (), (), (), ())

instance Arbitrary Tagged where
  arbitrary
    = elements
    [ SingleTagged
    , AllTagged
    ]

instance Arbitrary Addressable where
  arbitrary
    = elements
    [ NoFrameAddress
    , HasFrameAddress
    ]

instance Arbitrary AckRequired where
  arbitrary
    = elements
    [ NoAckRequired
    , AckRequired
    ]

instance Arbitrary ResRequired where
  arbitrary
    = elements
    [ NoResRequired
    , ResRequired
    ]

instance (WithSize a, Arbitrary a) => Arbitrary (Packet a) where
  arbitrary
    = do
    p <- Packet
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

    pure $ p { pFrame = (pFrame p) { fSize = size p } }

instance Arbitrary Frame where
  arbitrary
    = Frame
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary FrameAddress where
  arbitrary
    = FrameAddress
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ProtocolHeader where
  arbitrary
    = ProtocolHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

--instance Arbitrary (LifxProtocol a) where
--  arbitrary = genericArbitraryU
---- Carries a Proxy with a bunch of dictionaries
--data ProxyA where
--  ProxyA :: (Eq a, Show a, Binary a, WithSize a, Arbitrary a) => Proxy a -> ProxyA
--
---- | The functor identity law.
--prop_mapIdentityLaw :: (Eq a, Show a, Binary a, WithSize a)
--                    => Proxy a
--                    -> a
--                    -> Bool
--prop_mapIdentityLaw _ x = ((Bin.decode . Bin.encode) x) == x
--
--verify :: IO ()
--verify = forM_ types $ \ (ProxyA p) -> quickCheck $ prop_mapIdentityLaw p
--  where
--    types :: [ ProxyA ]
--    types =
--      [ ProxyA (Proxy :: Proxy (Packet GetService))
--      ]

mkTestDiscoveryPacket
  :: Sequence
  -> Packet GetService
mkTestDiscoveryPacket nextSeq
  = mkPacket
  AllTagged
  uniqueSource
  (word64leToTarget 0)
  NoAckRequired
  NoResRequired
  nextSeq
  (DeviceMessageType GetServiceMessage)
  GetService

main
  :: IO ()
main = hspec $ do
  describe "Packets" $ do
    it "disovery packet encodes correctly (critical)" $
      property $ \x -> do
        let
          seqEnc = BSL16.encode $ Bin.encode x
        (BSL16.encode $ Bin.encode $ mkTestDiscoveryPacket (Sequence x)) `shouldBe` ("24000034d2040000000000000000000000000000000000" <> seqEnc <> "000000000000000002000000")

--    it "encoding and decoding roundtrips" $
--      property $ \x -> (BSL16.encode $ Bin.encode x) `shouldBe` "24000034d204000000000000000000000000000000000000000000000000000002000000"
--                                                                                                                                    1000000000000000002000000
--                                                                                                                                   11000000000000000002000000
--                                                                                                                                   ff000000000000000002000000
