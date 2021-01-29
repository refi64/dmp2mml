-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow
import DMP2MML.DMPParser
import DMP2MML.MMLPrinter
import qualified Data.Binary.Parser as BP
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import Options.Applicative
import System.Exit
import qualified Text.PrettyPrint.Boxes as BX

data DMP2MML = DMP2MML
  { fileOpt :: String
  , idOpt :: Integer
  , descriptionOpt :: Maybe String
  }

dmp2mml = do
  fileOpt <- strArgument (metavar "FILE")
  idOpt <-
    option auto $
      long "id" <> short 'i'
        <> value 1
        <> showDefault
        <> help "The FM instrument ID to print"
  descriptionOpt <-
    optional . strOption $
      long "description"
        <> short 'd'
        -- <> value Nothing
        <> help "A description of the instrument"
  return DMP2MML{..}

trimTrailingWhitespace = unlines . map (dropWhileEnd isSpace) . lines

run DMP2MML{fileOpt, idOpt, descriptionOpt} = do
  dmp <-
    BS.readFile fileOpt
      >>= ( BP.parseOnly dmpParser
              >>> either (die . ("Failed to parse dmp file: " ++)) return
          )
  let text = BX.render $ printInstrument idOpt descriptionOpt dmp
  putStrLn $ trimTrailingWhitespace text

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info (dmp2mml <**> helper) $
        fullDesc <> progDesc "Convert a .dmp preset into ctrmml format"
