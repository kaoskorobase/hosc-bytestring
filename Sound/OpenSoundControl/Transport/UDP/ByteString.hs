-- | OSC over UDP implementation.
module Sound.OpenSoundControl.Transport.UDP.ByteString
  ( UDP
  , udpSocket
  , udpPort
  , openUDP
  , udpServer
  , sendTo
  , recvFrom
  ) where

import           Control.Monad (liftM, void)
import qualified Data.ByteString.Lazy as B
import           Network.Socket (SockAddr, Socket)
import qualified Network.Socket.ByteString as N
import qualified Network.Socket.ByteString.Lazy as NL
import           Sound.OpenSoundControl.OSC (OSC, decodeOSC)
import           Sound.OpenSoundControl.OSC.Builder (encodeOSC, encodeOSC')
import           Sound.OpenSoundControl.Transport (Transport(..))
import qualified Sound.OpenSoundControl.Transport.UDP as OSC

-- | The UDP transport handle data type.
newtype UDP = UDP { unUDP :: OSC.UDP } deriving (Eq, Show)

udpSocket :: UDP -> Socket
udpSocket = OSC.udpSocket . unUDP

udpPort :: Integral n => UDP -> IO n
udpPort = OSC.udpPort . unUDP

instance Transport UDP where
   send  u msg = void $ NL.send (udpSocket u) (encodeOSC msg)
   recv  u = liftM decodeOSC (NL.recv (udpSocket u) 8192)
   close u = close (unUDP u)

-- | Make a UDP connection.
openUDP :: String -> Int -> IO UDP
openUDP host port = liftM UDP $ OSC.openUDP host port

-- | Trivial udp server.
udpServer :: String -> Int -> IO UDP
udpServer host port = liftM UDP $ OSC.udpServer host port

sendTo :: UDP -> OSC -> SockAddr -> IO ()
sendTo u o a = void $ N.sendTo (udpSocket u) (encodeOSC' o) a

recvFrom :: UDP -> IO (OSC, SockAddr)
recvFrom u = do
    (s, a) <- N.recvFrom (udpSocket u) 8192
    let o = decodeOSC (B.fromChunks [s])
    return (o, a)
