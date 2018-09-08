{-# LANGUAGE OverloadedStrings #-}
module Parse
    ( Person(..)
    , parse
    ) where

import Control.Applicative
import Data.Vector 

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO  as T

import Data.Text as T
import Data.Text (Text)

import Data.Csv

data Person = Person
    { nome   :: !Text
    , salario :: !Int
    } deriving Show

instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "nome" <*> r .: "salario"


updateHead :: Text -> [Text] -> [Text]
updateHead _ [] = []
updateHead h (_:vals) = h:vals

changeHeaders :: FilePath -> Text -> FilePath -> IO ()
changeHeaders path content opath = do
    input <- T.readFile path
    content <-  T.unlines . updateHead content . T.lines <$> T.readFile path
    T.writeFile opath content

getData :: FilePath -> IO (Maybe (Vector Person))
getData path  = do
    cdvData <- BL.readFile path 
    case decodeByName cdvData of
        Left r -> putStrLn r >> pure Nothing
        Right (_,v) -> pure (Just v)

parse :: FilePath -> FilePath -> Text -> IO (Maybe (Vector Person))
parse i o headers = changeHeaders i headers o >> getData o
