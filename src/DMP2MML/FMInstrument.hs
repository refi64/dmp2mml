-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
module DMP2MML.FMInstrument where

import Data.Vector.Fixed.Boxed
import Data.Word

data FMOperator = FMOperator
  { _mult :: Word8
  , _tl :: Word8
  , _ar :: Word8
  , _dr :: Word8
  , _sl :: Word8
  , _rr :: Word8
  , _am :: Word8
  , _rs :: Word8
  , _dt :: Word8
  , _d2r :: Word8
  , _ssgeg :: Word8
  }

data FMInstrument = FMInstrument
  { _lfo :: Word8
  , _fb :: Word8
  , _alg :: Word8
  , _lfo2 :: Word8
  , _operators :: Vec4 FMOperator
  }
