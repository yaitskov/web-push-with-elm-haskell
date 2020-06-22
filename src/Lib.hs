{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Exception (try, displayException)
import Control.Concurrent.MVar
import Control.Lens ((&), (.~))
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Text as T
import Data.Word
import qualified Network.HTTP.Client as H
import Network.Wai.Handler.Warp

import Servant
import System.Clock
import Web.WebPush

data WebPushSubscription = WebPushSubscription
    { _wpsUrl :: T.Text
    , _wpsAuth :: T.Text
    , _wpsP256dh :: T.Text
    } deriving (Show, Eq)

instance FromJSON WebPushSubscription where
    parseJSON = withObject "" $ \o ->
                  WebPushSubscription
                             <$> o .: "url"
                             <*> o .: "auth"
                             <*> o .: "p256dh"


data WwdApp = WwdApp
    { _vapidKeys :: VAPIDKeysMinDetails
    , _waWpSubscriptions :: MVar [WebPushSubscription]
    , _waHttpManager :: H.Manager
    }

type API = "public-key" :> Get '[JSON] [Word8]
    :<|> "web-push-subscription"
             :> ReqBody '[JSON] WebPushSubscription
             :> Post '[JSON] ()
    :<|> "broadcast-msg-over-web-push"
             :> ReqBody '[JSON] T.Text
             :> Post '[JSON] Int
    :<|> "hello" :> Get '[PlainText] String
    :<|> Raw

type AppT = ReaderT WwdApp IO

staticServer :: ServerT Raw m
staticServer = serveDirectoryFileServer "."

initState :: IO WwdApp
initState = WwdApp
            <$> generateVAPIDKeys
            <*> newMVar []
            <*> H.newManager H.defaultManagerSettings

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

publicKey :: AppT [Word8]
publicKey = do
  st <- ask
  return . vapidPublicKeyBytes . readVAPIDKeys . _vapidKeys $ st

registerWebPushSubscription :: WebPushSubscription -> AppT ()
registerWebPushSubscription newWps = do
  st <- ask
  liftIO $ modifyMVar_ (_waWpSubscriptions st)
                 $ \wpss -> return $ newWps : wpss

broadcastMsgOverWebPush :: T.Text -> AppT Int
broadcastMsgOverWebPush bodyMessage = do
  st <- ask
  now <- liftIO $ getTime Realtime
  let keys = readVAPIDKeys $ _vapidKeys st
  counter <- liftIO $ newMVar 0
  wpSubs <- liftIO . readMVar $ _waWpSubscriptions st
  let payloadTitle = "Text Message " <> T.pack (show $ (nsec now) `mod` 1000)
  let payloadMsg = PushNotificationMessage
                   { title = payloadTitle
                   , body = bodyMessage
                   , icon = ""
                   , url = ""
                   , tag = "tag1"
                   }
  liftIO . forM_  wpSubs $ \wps -> do
    let pushNoti = mkPushNotification (_wpsUrl wps)
                   (_wpsP256dh wps) (_wpsAuth wps)
    resp <- sendPushNotification keys (_waHttpManager st)
            (pushNoti & pushSenderEmail .~ "dyaitskov@gmail.com"
                      & pushMessage .~ payloadMsg)
    case resp of
      Left e -> putStrLn $ "Bad subscription " ++ show wps
                ++ " due error: " ++ show e
      Right () -> do
                modifyMVar_ counter (\c -> return $ c + 1)
                putStrLn $ "Subscription " ++ show wps ++ " is notified"
    return ()
  liftIO $ readMVar counter

app :: WwdApp -> Application
app appSt = serve api (hoistServer api
                       (trans appSt)
                       (publicKey
                       :<|> registerWebPushSubscription
                       :<|> broadcastMsgOverWebPush
                       :<|> hello
                       :<|> staticServer))

startApp :: IO ()
startApp = do
  st <- initState
  runSettings serverSettings $ app st
