{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, LiberalTypeSynonyms, DeriveGeneric #-}

import Servant.Record.TH
import Servant.Record
import Servant.API
import Servant.Client
import Servant.Server

type TestAPI = "lists" :> QueryParam "start" Int :> QueryParam "count" Int :> Get '[JSON] [String]
           :<|> "lists" :> ReqBody '[JSON] String :> Post '[JSON] Int
           :<|> "lists" :> Capture "listid" Int :> Get '[JSON] Bool
           :<|> "lists" :> "by-name" :> Capture "name" String :> Get '[JSON] Bool
           :<|> "lists" :> Capture "listid" Int :> "subscribers" :> Get '[JSON] [(Int, String)]
           :<|> "lists" :> Capture "listid" Int :> "delete" :> ReqBody '[JSON] Int :> Post '[JSON] ()


servantRecords "Test" ''TestAPI [(''Server, "Sv"), (''Client, "")] [ "listList"
                                                                   , "listCreate"
                                                                   , "listShow"
                                                                   , "listShowName"
                                                                   , "listSubscribers"
                                                                   , "listDelete"
                                                                   ]

test1 :: Client TestAPI -> Test
test1 = fromAlts

test2 :: TestSv -> Server TestAPI
test2 = toAlts

main = do
  putStrLn "Success!"
