{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Exception (try, displayException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader

import qualified Data.ByteString.Char8 as BS
import Network.Wai.Handler.Warp
import Servant


data WwdApp = WwdApp
type API = "hello" :> Get '[PlainText] String :<|> Raw

type AppT = ReaderT WwdApp IO

staticServer :: ServerT Raw m
staticServer = serveDirectoryFileServer "."

initState :: IO WwdApp
initState = return WwdApp

trans :: WwdApp -> AppT x -> Handler x
trans st rdr = Handler $ liftIO (try (runReaderT rdr st)) >>=
  \case
    Left e -> do
      liftIO $ Prelude.putStrLn $ "My Exception " ++ displayException e
      throwError e
    Right ok -> return ok


serverSettings :: Settings
serverSettings =
    setTimeout 3
      (setServerName (BS.pack "web-watch-dog")
       (setPort 8080
         (setBeforeMainLoop
          (putStrLn "wwd is on port 8080...")
          defaultSettings)))

api :: Proxy API
api = Proxy

hello :: AppT String
hello = return "Hello world"

app :: WwdApp -> Application
app appSt = serve api (hoistServer api
                       (trans appSt)
                       (hello :<|> staticServer))

startApp :: IO ()
startApp = do
  st <- initState
  runSettings serverSettings $ app st
