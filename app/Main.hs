{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main
  ( main
  ) where

import           Import
import           Options.Generic (ParseRecord, getRecord)
import qualified Paths_simsim
import           RIO.Process
import           Run
import Prelude (print)

instance ParseRecord Options

main :: IO ()
main = print "Hello"
