{-# LANGUAGE ScopedTypeVariables #-}

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import           Sound.OpenSoundControl (OSC)
import           Sound.OpenSoundControl.Arbitrary ()
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.OpenSoundControl.ByteString as BOSC

tests :: [Test]
tests =
    [ testGroup "Sound.OpenSoundControl.ByteString"
        [ testProperty "encodeOSC" $ \(osc :: OSC) -> OSC.encodeOSC osc == BOSC.encodeOSC osc
        ]
    ]

main :: IO ()
main = defaultMain tests
