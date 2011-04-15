-- | OSC over UDP implementation.
module Sound.OpenSoundControl.Transport.ByteString.UDP ( UDP(udpSocket)
                                            , openUDP
                                            , udpServer
                                            , udpPort
                                            , sendTo, recvFrom ) where

import qualified Blaze.ByteString.Builder as B
import Control.Monad
import Data.ByteString.Lazy as B
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NS
import qualified Network.Socket.ByteString.Lazy as NB
import Sound.OpenSoundControl.OSC hiding (encodeOSC)
import Sound.OpenSoundControl.OSC.Builder
import Sound.OpenSoundControl.Transport

-- | The UDP transport handle data type.
data UDP = UDP { udpSocket :: N.Socket }
           deriving (Eq, Show)

instance Transport UDP where
   send  (UDP fd) msg = NB.send fd (encodeOSC msg) >> return ()
   recv  (UDP fd) = liftM decodeOSC (NB.recv fd 8192)
   close (UDP fd) = N.sClose fd

-- | Make a UDP connection.
openUDP :: String -> Int -> IO UDP
openUDP host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  a  <- N.inet_addr host
  let sa = N.SockAddrInet (fromIntegral port) a
  N.connect fd sa
  -- N.setSocketOption fd N.RecvTimeOut 1000
  return (UDP fd)

-- | Trivial udp server.
udpServer :: String -> Int -> IO UDP
udpServer host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  a  <- N.inet_addr host
  let sa = N.SockAddrInet (fromIntegral port) a
  N.bindSocket fd sa
  return (UDP fd)

sendTo :: UDP -> OSC -> N.SockAddr -> IO ()
sendTo (UDP fd) o a = do
  _ <- NS.sendTo fd (B.toByteString (buildOSC o)) a
  return ()

recvFrom :: UDP -> IO (OSC, N.SockAddr)
recvFrom (UDP fd) = do
  (s, a) <- NS.recvFrom fd 8192
  let o = decodeOSC (B.fromChunks [s])
  return (o, a)

udpPort :: Integral n => UDP -> IO n
udpPort (UDP fd) = fmap fromIntegral (N.socketPort fd)
