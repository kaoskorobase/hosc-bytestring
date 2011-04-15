module Sound.OpenSoundControl.Arbitrary () where

import Control.Applicative
import Sound.OpenSoundControl (Datum(..), OSC(..), Time(..))
import Test.QuickCheck

instance Arbitrary Time where
    arbitrary = oneof [
        UTCr <$> arbitrary
      , NTPr <$> arbitrary
      , NTPi <$> arbitrary
      ]

instance Arbitrary Datum where
    arbitrary = oneof [
        Int <$> arbitrary
      , Float <$> arbitrary
      , Double <$> arbitrary
      , String <$> resize 128 arbitrary
      , Blob <$> resize 128 arbitrary
      , TimeStamp <$> arbitrary
      , Midi <$> arbitrary ]

genMessage :: Gen OSC
genMessage = Message <$> ("/"++) <$> resize 32 (listOf1 arbitrary) <*> resize 32 (listOf1 arbitrary)

instance Arbitrary OSC where
    arbitrary = oneof [
        genMessage
      , Bundle <$> arbitrary <*> resize 32 (listOf1 genMessage) ]
