{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Lib

import Test.Hspec
import Test.QuickCheck
import qualified Data.Binary as Bin

instance Arbitrary Direction where
  arbitrary 

instance Arbitrary a => Arbitrary (Packet a) where
  arbitrary
    = Packet
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

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

main
  :: IO ()
main = hspec $ do
  describe "Packets" $ do
    it "encoding and decoding roundtrips" $
      property $ \p -> (Bin.decode . Bin.encode) p `shouldBe` (p :: Packet Int)
