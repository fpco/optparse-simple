{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Options.Applicative.Simple (simpleVersion)
import qualified Paths_optparse_simple as Meta

main :: IO ()
main = putStrLn $(simpleVersion Meta.version)