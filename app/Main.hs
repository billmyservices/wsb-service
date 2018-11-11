{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Main where

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost)
import System.Environment
import WSB.Args
import WSB.Client
import WSB.Client.Counter
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Conversion as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

fromText :: Text -> ByteString
fromText = encodeUtf8

fromString :: String -> ByteString
fromString = fromText . Text.pack

parse :: (Read a, Monad m) => String -> ExceptT ByteString m a
parse n = case reads n of
              [(x, "")] -> return x
              _         -> throwError $ "Cannot parse `" <> fromString n <> "`"

lookupEnv' :: String -> ExceptT ByteString IO String
lookupEnv' n = lift (lookupEnv n) >>= \case
  Just  s -> return s
  Nothing -> throwError $ "You must define the " <> fromString n <> " environment variable"

app :: AuthReq -> Application
app auth rq response = do
  result <- runExceptT $ case (pathInfo rq) of
              [ct, c, "read"] -> (fromString . show . WSB.Client.Counter.value) <$> counterRead (CounterReq (CounterTypeReq auth ct) c)
              [ct, c,     v ] -> parse (Text.unpack v) >>= counterPost (CounterReq (CounterTypeReq auth ct) c) >> return "ok"
              xs              -> throwError $ "Bad path: '" <> fromText (Text.intercalate "</>" xs)
  case result of
    Left  e -> response $ ko $ LBS.fromStrict e
    Right r -> response $ ok $ LBS.fromStrict r

textPlain = ("Content-Type", "text/plain")

ko = responseLBS status403 [textPlain]
ok = responseLBS status200 [textPlain]

main = do

  eauth <- runExceptT $ do
              -- configuration from environment
              wsbUrl     <- lookupEnv' "WSB_URL"
              wsbUserId  <- lookupEnv' "WSB_USERID" >>= parse
              wsbSkey    <- lookupEnv' "WSB_SKEY"
              wsbPort    <- lookupEnv' "WSB_PORT" >>= parse

              -- strict hmac parsing
              hmac <- case B64.decode (BS.toByteString' wsbSkey) of
                        Right x -> return x
                        Left  e -> throwError $ "Cannot decode the secret key: " <> fromString e

              auth <- authReq wsbUserId hmac wsbUrl

              return (wsbPort, auth)

  case eauth of
    Left e             -> putStrLn $ "ERROR: " <> (Text.unpack . decodeUtf8) e
    Right (port, auth) -> runSettings (setPort port $ setHost "127.0.0.1" $ defaultSettings) $ app auth

