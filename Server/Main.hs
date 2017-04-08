{-# LANGUAGE OverloadedStrings #-}

module Server.Main
( server
) where

import qualified Control.Concurrent as Conc
import qualified Control.Exception  as Exc
import Control.Monad

import qualified Data.Text            as Text
import qualified Data.ByteString.Lazy as BL
import Data.Binary

import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

import qualified Safe

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

nextId :: State -> ClientId
nextId = maybe 0 (+ 1) . Safe.maximumMay . fmap fst

connectClient :: WS.Connection -> Conc.MVar State -> IO ClientId
connectClient conn stateRef = Conc.modifyMVar stateRef $ \state -> do
    let newClientId = nextId state
    return ((newClientId, conn) : state, newClientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = filter ((/= clientId) . fst)

disconnectClient :: ClientId -> Conc.MVar State -> IO ()
disconnectClient clientId stateRef = Conc.modifyMVar_ stateRef $ return . withoutClient clientId

broadcast :: ClientId -> Conc.MVar State -> Text.Text -> IO ()
broadcast clientId stateRef msg = do
    clients <- Conc.readMVar stateRef
    let otherClients = withoutClient clientId clients
    forM_ otherClients $ flip WS.sendTextData msg . snd

listen :: WS.Connection -> ClientId -> Conc.MVar State -> IO ()
listen conn clientId stateRef = forever $ WS.receiveData conn >>= broadcast clientId stateRef

wsApp :: Conc.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
    conn <- WS.acceptRequest pendingConn
    clientId <- connectClient conn stateRef
    WS.forkPingThread conn 30
    Exc.finally (listen conn clientId stateRef) (disconnectClient clientId stateRef)

notFound :: Wai.Response
notFound = Wai.responseLBS Http.status404 [("Content-Type", "text/plain")] "404 - Page Not Found"

index :: Wai.Response
index = Wai.responseFile Http.status200 [("Content-Type", "text/html")] "client/index.html" Nothing

httpApp :: Wai.Application
httpApp request respond = respond $ case Wai.rawPathInfo request of
                                "/" -> index
                                _   -> notFound

server :: IO ()
server = do
    putStrLn "Starting server on port 3000"
    state <- Conc.newMVar []
    Warp.run 3000 $ WS.websocketsOr
        WS.defaultConnectionOptions (wsApp state) httpApp
