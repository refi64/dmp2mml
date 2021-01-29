-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module DMP2MML.MMLPrinter (printInstrument) where

import DMP2MML.FMInstrument
import Data.Foldable
import Text.PrettyPrint.Boxes ((<+>))
import qualified Text.PrettyPrint.Boxes as BX

data Field a f where
  Field :: Show f => String -> (a -> f) -> Field a f

printFields :: (Show f) => [Field a f] -> [a] -> [BX.Box]
printFields fields xs = map printColumn fields
  where
    printColumn (Field name getter) =
      BX.vcat BX.right $ BX.text name : map (BX.text . show . getter) xs

opFields =
  [ Field "AR" _ar
  , Field "DR" _dr
  , Field "SR" _d2r
  , Field "RR" _rr
  , Field "SL" _sl
  , Field "TL" _tl
  , Field "KS" _rs
  , Field "ML" _mult
  , Field "DT" _dt
  , Field "SSG" _ssgeg
  ]

instrumentFields =
  [ Field "ALG" _alg
  , Field "FB" _fb
  ]

printInstrument idNumber description fm =
  let is = printFields instrumentFields [fm]
      os = printFields opFields . toList $ _operators fm

      -- We grab the first few operator columns to align them with the two
      -- instrument field columns.
      firstCols = zipWith (\a b -> BX.vcat BX.right [a, b]) is os
      restCols = drop (length firstCols) os

      instrumentRows = BX.rows $ head is
      operatorRows = BX.rows $ head os
      commentCol =
        BX.vcat
          BX.top
          [ BX.char ';'
          , BX.emptyBox (instrumentRows - 1) 0
          , BX.char ';'
          , BX.emptyBox (operatorRows - 1) 0
          ]

      header = BX.text $ "@" ++ show idNumber ++ " fm"

      descriptionBox = fmap (BX.text . (" ; " ++)) description
      addDescription = maybe id (flip (<+>)) descriptionBox

      definition = BX.hsep 1 BX.bottom $ commentCol : firstCols ++ restCols
   in BX.vcat
        BX.top
        [ addDescription header
        , BX.nullBox <+> definition
        ]
