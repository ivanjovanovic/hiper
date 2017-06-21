{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

import qualified Data.Hiper as H

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main = do
  hiper <- H.loadConfig hiperConfig
  port <- H.lookup hiper "port" :: IO (Maybe Int)
  maybe error run port
  where
    error = print "error starting, no port defined"
    run p = do
      ref <- newIORef 0
      spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
      runSpock p (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

hiperConfig :: H.HiperConfig
hiperConfig = H.addDefault H.emptyConfig "port" (H.Number 9000)
