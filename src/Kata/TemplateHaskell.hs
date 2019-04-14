{-# LANGUAGE TemplateHaskell  #-}
module Kata.TemplateHaskell where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q
                           , Exp
                           , varP
                           , varE
                           , lamE
                           , newName)

genId :: Q Exp
genId = do
  x <- newName "x"
  lamE [varP x] (varE x)

-- :set -XTemplateHaskell
-- show $ $(genId) 7

-- let genIdFn = TH.runQ genId

-- :t genIdFn
-- genIdFn :: Language.Haskell.TH.Syntax.Quasi m => m Exp

-- :t $genIdFn
-- $genIdFn :: p -> p
