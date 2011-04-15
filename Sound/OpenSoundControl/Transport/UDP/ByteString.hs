-- | OSC over UDP implementation.
module Sound.OpenSoundControl.Transport.UDP.ByteString
  ( sendTo
  , recvFrom
  ) where

import           Control.Monad (void)
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import           Network.Socket (SockAddr)
import qualified Network.Socket.ByteString as N
import           Sound.OpenSoundControl.OSC (OSC, decodeOSC)
import           Sound.OpenSoundControl.OSC.Builder (buildOSC)
import           Sound.OpenSoundControl.Transport.UDP (UDP, udpSocket)

sendTo :: UDP -> OSC -> SockAddr -> IO ()
sendTo u o a = void $ N.sendTo (udpSocket u) (B.toByteString (buildOSC o)) a

recvFrom :: UDP -> IO (OSC, SockAddr)
recvFrom u = do
    (s, a) <- N.recvFrom (udpSocket u) 8192
    let o = decodeOSC (B.fromChunks [s])
    return (o, a)
