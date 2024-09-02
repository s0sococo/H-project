module Main where

import Control.Monad (unless)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as S

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       putStrLn "Enter expression (or 'exit'):"
       loop sock
       close sock
            where loop sock = do
                    input <- getLine
                    unless (input == "exit") $ do
                        sendAll sock $ S.pack input
                        msg <- recv sock 1024
                        putStr "= "
                        S.putStrLn msg
                        loop sock