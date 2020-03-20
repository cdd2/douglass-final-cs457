{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.Default (def)
import Text.HTML.Scalpel

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP

-- Create a new manager settings based on the default TLS manager that updates
-- the request headers to include a custom user agent.
managerSettings :: HTTP.ManagerSettings
managerSettings = HTTP.tlsManagerSettings {
  HTTP.managerModifyRequest = \req -> do
    req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
    return $ req' {
      HTTP.requestHeaders = (HTTP.hUserAgent, "My User Agent")
                          : HTTP.requestHeaders req'
    }
}

main :: IO ()
main = handleArgs ["https://www.fandango.com/movies-in-theaters"]

handleArgs :: [String] -> IO ()
handleArgs [url] = listMovieNames url
handleArgs _     = putStrLn "usage: custom-user-agent URL"

listMovieNames :: URL -> IO ()
listMovieNames url = do
    manager <- Just <$> HTTP.newManager managerSettings
    images <- scrapeURLWithConfig (def { manager }) url $ texts movieTitleSelector
    maybe printError printImages images
    where
        printError = putStrLn "ERROR: Could not scrape the URL!"
        printImages = mapM_ putStrLn

movieTitleSelector :: Selector
movieTitleSelector = "h4" @: [hasClass "mlp__listings-section-item-title"]