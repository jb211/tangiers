{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified Data.Aeson.Parser


type FundAPI1 = "funds" :> Get '[JSON] [Fund]

data Fund = Fund {
	name :: String,
	age  :: Int,
	email :: String,
	inception_date :: Day
} deriving (Eq, Show, Generic)

instance ToJSON Fund

funds1 :: [Fund]
funds1 =
	[ Fund "Credit Opportunities" 10 "creditOpps@co.com" (fromGregorian 2002 3 1)
	 ,Fund "Long-Short Alpha Plus" 10 "lseAlpha@hf.com"  (fromGregorian 2008 10 8)
	]

server1 :: Server FundAPI1
server1 = return funds1

fundAPI :: Proxy FundAPI1
fundAPI = Proxy

app1 :: Application
app1 = server fundAPI server1

main :: IO ()
main = run 8081 app1