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

data Version = V0 | V9 | V11

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

parseVersion = do
  version <- BP.anyWord8
  case version of
    0 -> return V0
    9 -> return V9
    11 -> return V11
    _ ->
      fail $ "Unsupported DMP version " ++ show version ++ ", try re-saving it"

parseGenesisSystem =
  BP.word8 genesisSystem <|> fail "Unsupported system"
  where
    genesisSystem = 2

parseInstrumentMode =
  BP.word8 fmInstrumentMode <|> fail "Unsupported instrument mode"
  where
    fmInstrumentMode = 1

parseVersionHeader V0 = return ()
parseVersionHeader V9 = do
  parseInstrumentMode
  -- The next byte's meaning is unknown, but it's usually 0.
  BP.anyWord8
  return ()
parseVersionHeader V11 = do
  parseGenesisSystem
  parseInstrumentMode

dmpParser = do
  parseVersion >>= parseVersionHeader
  parseFM
