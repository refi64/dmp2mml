-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE RecordWildCards #-}

module DMP2MML.DMPParser (
  dmpParser,
) where

import Control.Applicative
import qualified Data.Binary.Parser as BP
import Data.Bits
import Data.Vector.Fixed
import Data.Word

import DMP2MML.FMInstrument

parseFMOperator = do
  _mult <- BP.anyWord8
  _tl <- BP.anyWord8
  _ar <- BP.anyWord8
  _dr <- BP.anyWord8
  _sl <- BP.anyWord8
  _rr <- BP.anyWord8
  _am <- BP.anyWord8
  _rs <- BP.anyWord8
  _dt <- BP.anyWord8
  _d2r <- BP.anyWord8
  _ssgeg <- parseSSGEG <$> BP.anyWord8
  return FMOperator{..}
  where
    ssgegBit = 3
    parseSSGEG raw
      | raw `testBit` ssgegBit = raw `clearBit` ssgegBit
      | otherwise = 0

parseFM = do
  _lfo <- BP.anyWord8
  _fb <- BP.anyWord8
  _alg <- BP.anyWord8
  _lfo2 <- BP.anyWord8
  _operators <- replicateM parseFMOperator
  return FMInstrument{..}

dmpParser = do
  BP.word8 dmpFileVersion <|> fail "Wrong DMP file version, try re-saving it"
  BP.word8 genesisSystem <|> fail "Unsupported system"
  BP.word8 fmInstrumentMode <|> fail "Unexpected instrument mode"
  parseFM
  where
    dmpFileVersion = 11
    genesisSystem = 2
    fmInstrumentMode = 1
