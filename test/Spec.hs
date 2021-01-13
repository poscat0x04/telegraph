{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Effect
import Control.Effect.Reader
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.TLS
import Web.Telegraph.API
import Web.Telegraph.Types

main :: IO ()
main = do
  manager <- newTlsManager
  runM $
    errorToIOThrowing @TelegraphError $
      errorToIOThrowing @HttpException $
        runReader manager $
          runTelegraph "b968da509bb76866c35425099bc0989a5ec3b32997d55286c657e6994bbb" $ do
            pl <- getPageList 0 3
            embed $ print pl
            Page {path} <- createPage "test" $ pure $ Element $ NodeElement "p" [] [Content "Hello"]
            p <- getPage path
            embed $ print p
            _ <- editPage path "test" $ pure $ Element $ NodeElement "p" [] [Content "Bye"]
            p' <- getPage path
            embed $ print p'
            a <- getAccountInfo
            embed $ print a
