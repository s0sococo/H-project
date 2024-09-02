module Main where

import Control.Monad (unless)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as S

import Parsers

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing
                    (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       bind sock (addrAddress serveraddr)
       listen sock 1
       (conn, _) <- accept sock
       talk conn >> close conn >> close sock

    where
      talk :: Socket -> IO ()
      talk conn = do
             msg <- recv conn 1024
             unless (S.null msg) $ do
                let res = case apply expr $ S.unpack msg of
                            [(val, "")] -> show val
                            _ -> "Parse error"
                sendAll conn (S.pack res) >> talk conn