{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Fund json
    name String
    age  Int
    deriving Show
|]

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
    json $
      object
      ["result" .= String "failure"
      , "error" .= object ["code" .= code, "message" .= message]
      ]


main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "funds" $ do
    allFunds <- runSQL $ selectList [] [Asc FundId]
    json allFunds
  post "funds" $ do
    maybeFund <- jsonBody :: ApiAction (Maybe Fund)
    case maybeFund of
        Nothing -> errorJson 1 "Failed to parse request body as fund"
        Just theFund -> do
            newId <- runSQL $ insert theFund
            json $ object ["result" .= String "success", "id" .= newId]
  get ("funds" <//> var) $ \fundId -> do
    maybeFund <- runSQL $ P.get fundId :: ApiAction (Maybe Fund)
    case maybeFund of
        Nothing -> errorJson 2 "Could not find fund with id"
        Just theFund -> json theFund