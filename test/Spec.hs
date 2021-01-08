{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Reader
import Network.HTTP.Client.TLS
import Web.Telegraph.API
import Web.Telegraph.Types

main :: IO ()
main = do
  manager <- newTlsManager
  flip runReaderT manager $
    runTelegraph "b968da509bb76866c35425099bc0989a5ec3b32997d55286c657e6994bbb" $ do
      pl <- getPageList 0 3
      liftIO $ print pl
      Page {path} <- createPage "test" $ pure $ Element $ NodeElement "p" [] [Content "Hello"]
      p <- getPage path
      liftIO $ print p
      _ <- editPage path "test" $ pure $ Element $ NodeElement "p" [] [Content "Bye"]
      p' <- getPage path
      liftIO $ print p'
      a <- getAccountInfo
      liftIO $ print a
