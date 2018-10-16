{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Text (Text)
import Control.Monad.Reader

import Network.Wai.Internal (ResponseReceived(..))

import Servant
import Servant.API
import Servant.Client
import Servant.Client.Core


-- A simple API with two endpoints: one for reversing a string and one for
-- replicating it.
type API =
         "reverse"
             :> Capture "s" Text
             :> Get '[JSON] Text
    :<|> "replicate"
             :> Capture "n" Int
             :> Capture "s" Text
             :> Get '[JSON] [Text]

main :: IO ()
main = do
    putStrLn "hello world"
