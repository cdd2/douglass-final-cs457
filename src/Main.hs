{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Time

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
main = do
    --
    let fandangoBaseURL :: String
        fandangoBaseURL = "https://www.fandango.com/movies-in-theaters"

    start <- getCurrentTime
    let daysToSearch = 5
    let cinemarkBaseURL :: String
        cinemarkBaseURL = "https://www.cinemark.com/theatres/or-beaverton/century-16-cedar-hills?showDate="
    let cinemarkMovieTimes = getMovieTimesNearMeURLs cinemarkBaseURL daysToSearch start

    putStrLn "Movie Times Near Me"
    mapM_ listCinemarkMovieTimes cinemarkMovieTimes

    putStrLn "All Movies"
    listAllMovieNames fandangoBaseURL

listAllMovieNames :: URL -> IO ()
listAllMovieNames url = do
    manager <- Just <$> HTTP.newManager managerSettings
    images <- scrapeURLWithConfig (def { manager }) url $ texts fandangoMovieTitleSelector
    maybe printError printImages images
    where
        printError = putStrLn "ERROR: Could not scrape the URL!"
        printImages = mapM_ putStrLn

listCinemarkMovieTimes :: URL -> IO ()
listCinemarkMovieTimes url = do
    manager <- Just <$> HTTP.newManager managerSettings
    images <- scrapeURLWithConfig (def { manager }) url $ texts cinemarkMovieTimeSelector
    maybe printError printImages images
    where
        printError = putStrLn "ERROR: Could not scrape the URL!"
        printImages = mapM_ putStrLn

fandangoMovieTitleSelector :: Selector
fandangoMovieTitleSelector = "h4" @: [hasClass "mlp__listings-section-item-title"]

cinemarkMovieTimeSelector :: Selector
cinemarkMovieTimeSelector = "p" @: [hasClass "no-showtimes"]

getMovieTimesNearMeURLs :: String -> Int -> UTCTime -> [String]
getMovieTimesNearMeURLs baseURL daysToSearch start = take daysToSearch
    [baseURL ++ (formatTime defaultTimeLocale "%F" (addUTCTime (i * 3600) start)) | i <- [1..]]