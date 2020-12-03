{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}
module Api where

import           Data.Aeson
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics
import           Servant

data User = Ashton | Bob | Chuck deriving (Eq, Show, Generic, ToJSON)

type Api =
  BasicAuth "Private API" User :> PrivateApi
  :<|> PublicApi

type PrivateApi = "report" :> Get '[JSON] Report

type PublicApi = Raw

checkBasicAuth :: BasicAuthCheck User
checkBasicAuth = BasicAuthCheck $ \basicAuthData ->
  let username = decodeUtf8 (basicAuthUsername basicAuthData)
      password = decodeUtf8 (basicAuthPassword basicAuthData)
  in
  case (username, password) of
    ("ashton", "pw") -> pure $ Authorized Ashton
    ("ashton", _)    -> pure BadPassword
    ("bob", "pw")    -> pure $ Authorized Bob
    ("bob", _)       -> pure BadPassword
    ("chuck", "pw")  -> pure $ Authorized Chuck
    ("check", _)     -> pure BadPassword
    _                -> pure NoSuchUser

data MeatbarType = Beef | Bison | Lamb deriving (Eq, Show, Generic, ToJSON)

data Consumption
  = Consumption
  { consumptionMeatbarType :: !MeatbarType
  , consumptionNumConsumed :: !Int
  }

instance ToJSON Consumption where
  toJSON consumption = object
    [ "label" .= consumptionMeatbarType consumption
    , "y" .= consumptionNumConsumed consumption
    ]

data Report
  = Report
   { reportUser         :: !User
   , reportConsumptions :: ![Consumption]
   }

instance ToJSON Report where
  toJSON report = object
    [ "user" .= reportUser report
    , "consumptions" .= reportConsumptions report
    ]

privateApi :: User -> ServerT PrivateApi Handler
privateApi Ashton = pure $ Report
  { reportUser = Ashton
  , reportConsumptions =
    [ Consumption Beef 19
    , Consumption Bison 5
    , Consumption Lamb 37
    ]
  }
privateApi Bob = pure $ Report
  { reportUser = Bob
  , reportConsumptions =
    [ Consumption Lamb 99218
    ]
  }
privateApi Chuck = pure $ Report
  { reportUser = Chuck
  , reportConsumptions =
    [ Consumption Beef 10
    , Consumption Bison 21
    ]
  }

publicApi :: ServerT PublicApi Handler
publicApi = serveDirectoryFileServer "webapp/"

server :: ServerT Api Handler
server = privateApi :<|> publicApi
