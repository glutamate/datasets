{-# LANGUAGE OverloadedStrings #-}

module Numeric.Dataset.UCI where

import Network.HTTP
import Data.Csv
import System.FilePath
import System.Directory
import Data.Hashable
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Char (toUpper)
import Text.Read (readMaybe)
import Data.ByteString.Char8 (unpack)

type Dataset a = FilePath -- ^ Directory for caching downloaded datasets
                 -> IO [a]
type URL = String

-- |Retrieve a dataset
getDataset :: Dataset a -> IO [a]
getDataset ds = ds "/tmp/haskds"

csvDataset :: FromRecord a => URL -> Dataset a
csvDataset url cacheDir = do
  createDirectoryIfMissing True cacheDir
  let fnm = cacheDir </> "ds" <> show (hash url)
      parseFile contents = do
        case decode NoHeader contents of
          Right theData -> return $ V.toList theData
          Left err -> fail err
      castRequest :: Request String -> Request BL.ByteString
      castRequest r = Request (rqURI r) (rqMethod r) (rqHeaders r) ""

  ex <- doesFileExist fnm
  if ex
     then BL.readFile fnm >>= parseFile
     else do
       rsp <- simpleHTTP (castRequest $ getRequest url)
       bs <- getResponseBody rsp
       BL.writeFile fnm bs
       parseFile bs

dashToCamelCase :: String -> String
dashToCamelCase ('-':c:cs) = toUpper c : dashToCamelCase cs
dashToCamelCase (c:cs) = c : dashToCamelCase cs
dashToCamelCase [] = []

parseDashToCamelField :: Read a => Field -> Parser a
parseDashToCamelField s =
  case readMaybe (dashToCamelCase $ unpack s) of
    Just wc -> pure wc
    Nothing -> fail "unknown"
