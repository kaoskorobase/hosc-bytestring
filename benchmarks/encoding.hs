import Criterion.Main

import qualified Data.ByteString.Lazy as B
import Sound.OpenSoundControl (Datum(..), OSC(..), Time(..))
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.OpenSoundControl.ByteString as BOSC
import Sound.OpenSoundControl.NFData ()

main =
    defaultMain [
         bench "OSC"  $ nf (B.toChunks . OSC.encodeOSC)  b
       , bench "BOSC" $ nf (B.toChunks . BOSC.encodeOSC) b
       ]
    where
        m = Message "/fooblah" [Float 42, Int 16, String "yeah", Blob [0..128]]
        b = Bundle (NTPr pi) (replicate 12 m)
