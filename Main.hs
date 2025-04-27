{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)

import QuoteData
import Charts
import StatReport
import HtmlReport
import Params


main :: IO ()
main = cmdLineParser >>= work

generateReports :: (Functor t, Foldable t) =>
                   Params -> t QuoteData -> IO ()
generateReports Params {..} quotes = do
	unless silent $ putStr textRpt
	when chart $ plotChart title quotes chartFname
	saveHtml htmlFile htmlRpt
	where
		statInfo' = statInfo quotes
		textRpt = textReport statInfo'
		htmlRpt = htmlReport title quotes statInfo' [chartFname | chart]

		withCompany prefix = maybe mempty (prefix <>) company
		chartFname = unpack $ "chart" <> withCompany "_" <> ".svg"
		title = unpack $ "Historical Quotes" <> withCompany " for "

		saveHtml Nothing _ = pure ()
		saveHtml (Just f) html = BL.writeFile f html


work :: Params -> IO ()
work params = do
    csvData <- BL.readFile (fname params)
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, quotes) -> generateReports params quotes


-- decodeByName :: FromNamedRecord a
--                 => BL.ByteString
--                 -> Either String (Header, Vector a)


-- (Functor t, Foldable t) => t QuoteData


readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
	csvData <- BL.readFile fpath
	case decodeByName csvData of
		Left err -> error err
		Right (_, quotes) -> pure (toList quotes)
