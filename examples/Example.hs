{-# LANGUAGE DeriveGeneric, TypeOperators, DataKinds, LiberalTypeSynonyms,
             TemplateHaskell #-}

module Example where

import GHC.Generics (Generic)
import Control.Lens
import Data.Proxy
import Network.Wai
import Servant.API
import Servant.Client
import Servant.Server

import Servant.Record
import Servant.Record.TH

--
-- Test API
--

type MyApi =      "numbers" :> Get '[JSON] [Integer]
             :<|> "numbers" :> ReqBody '[JSON] Integer :> Post '[JSON] ()
             :<|> "numbers" :> Capture "number" Integer :> Delete '[JSON] ()

--
-- Example without TH
--

data MyServer = MyServer { svListNumbers :: NthT Server MyApi 0
                         , svAddNumber :: NthT Server MyApi 1
                         , svDeleteNumber :: NthT Server MyApi 2
                         }
              deriving (Generic)

myServer :: Application
myServer = serve (Proxy :: Proxy MyApi) ((undefined :: MyServer) ^. recordIso)

data MyClient = MyClient { clListNumbers :: NthT Client MyApi 0
                         , clAddNumber :: NthT Client MyApi 1
                         , clDeleteNumber :: NthT Client MyApi 2
                         }
              deriving (Generic)

myClient :: MyClient
myClient = client (Proxy :: Proxy MyApi) (BaseUrl Http undefined undefined) ^. from recordIso

--
-- Example with TH
--

servantRecord "MyServerTH" ''MyApi ''Server [ "svListNumbersTH"
                                            , "svAddNumberTH"
                                            , "svDeleteNumberTH"
                                            ]

myServerTH :: Application
myServerTH = serve (Proxy :: Proxy MyApi) ((undefined :: MyServerTH) ^. recordIso)

servantRecord "MyClientTH" ''MyApi ''Client [ "clListNumbersTH"
                                            , "clAddNumberTH"
                                            , "clDeleteNumberTH"
                                            ]

myClientTH :: MyClientTH
myClientTH = client (Proxy :: Proxy MyApi) (BaseUrl Http undefined undefined) ^. from recordIso
