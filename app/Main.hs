{-# LANGUAGE DataKinds #-}
module Main where

import           Api
import           Data.Proxy
import           Network.Wai.Handler.Warp             hiding (getPort)
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           System.Environment                   (lookupEnv)

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"

  let
    cfg = checkBasicAuth :. EmptyContext
    settings = setPort port defaultSettings

  runSettings settings . simpleCors . logStdoutDev $ serveWithContext
    (Proxy :: Proxy Api)
    cfg
    server
