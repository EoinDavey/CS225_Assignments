module Main where

import Network.Connection
import Network.HTTP.Conduit
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List.Split
import Control.Concurrent.Async
import System.IO

import qualified Data.ByteString.Lazy.Char8 as BL

smbcComic = "http://www.smbc-comics.com/comic/"

main :: IO ()
main = do
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    body <- getBody manager "http://www.smbc-comics.com/comic/archive"
    let doc = readString [withParseHTML yes, withWarnings no] (BL.unpack body)
    comics <- runX $ doc //> css "select" >>> hasAttrValue "name" (=="comic")
                         >>> css "option" >>> getAttrValue "value"
                         >>. filter (not . null)
    let comicUrls = map (smbcComic ++) comics
    --mapM_ (\x -> (getImageLink manager x) >>= (saveImage manager)) $ take 1 comicUrls
    forConcurrently_  (take 100 comicUrls)  (\x -> (getImageLink manager x) >>= (saveImage manager))

getBody:: Manager -> String -> IO BL.ByteString
getBody m url = do
    req <- parseRequest url
    res <- httpLbs req m
    return $ responseBody res

getImageLink:: Manager -> String -> IO String
getImageLink m url = do
    body <- getBody m url
    let doc = readString [withParseHTML yes, withWarnings no] (BL.unpack body)
    imageLink <- runX $ doc //> css "img" >>> hasAttrValue "id" (=="cc-comic") >>> getAttrValue "src"
    return (head imageLink)

saveImage:: Manager -> String -> IO()
saveImage m url = do
    img <- getBody m url
    let loc = "imgs/" ++ (last (splitOn "/" url))
    BL.writeFile loc img
