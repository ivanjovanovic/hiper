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
  case hiperConfig of
    Just conf -> do
      hiper <- H.loadConfig conf
      port <- H.lookup hiper "port" :: IO (Maybe Int)
      maybe error (run hiper) port
    Nothing -> putStrLn "Config is wrong"

  where
    error = print "error starting, no port defined"
    run h p = do
      ref <- newIORef 0
      spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
      runSpock p (spock spockCfg $ app h)

app :: H.Hiper -> SpockM () MySession MyAppState ()
app h =
    do get root $
         text "hello"

       get ("config") $ do
         port <- liftIO $ (H.lookup h "port" :: IO (Maybe Int))
         let result = case port of
               Just p -> p
               Nothing -> 0
         text $ T.pack (show result)


       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))


hiperConfig :: Maybe H.HiperConfig
hiperConfig = H.addDefault emptyConfig "port" (9000 :: Int)
  where
    emptyConfig = H.emptyConfig
      { H.hcPaths = [T.pack "", T.pack "/tmp"]
      , H.hcFile = (T.pack "test")
      , H.hcExtensions = [T.pack "yaml"]
      }
