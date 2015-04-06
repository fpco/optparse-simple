{-# LANGUAGE TemplateHaskell #-}

-- | Simple interface to program arguments.

module Options.Applicative.Simple
  ( module Options.Applicative.Simple
  , module Options.Applicative
  ) where

import Control.Monad.Trans.Writer
import Data.Version
import Development.GitRev (gitDirty, gitHash)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Options.Applicative

-- | Generate a simple options parser.
simpleOptions :: String                          -- ^ version string
              -> String                          -- ^ header
              -> String                          -- ^ program description
              -> Parser a                        -- ^ global settings
              -> Writer (Mod CommandFields b) () -- ^ commands (use 'addCommand')
              -> IO (a,b)
simpleOptions versionString h pd globalParser commandParser =
  execParser $
  info (helpOption <*> versionOption <*> config) desc
  where desc = fullDesc <> header h <> progDesc pd
        helpOption =
          abortOption ShowHelpText $
          long "help" <>
          help "Show this help text"
        versionOption =
          infoOption
            versionString
            (long "version" <>
             help "Show version")
        config =
          (,) <$> globalParser <*>
          subparser (execWriter commandParser)

-- | Generate a string like @Version 1.2, Git revision 1234@.
--
-- @$(simpleVersion …)@ @::@ 'String'
simpleVersion :: Version -> Q (TExp String)
simpleVersion version =
  fmap TExp
       [|concat ["Version "
                ,$(lift $ showVersion version)
                ,", Git revision "
                ,$gitHash
                ,if $gitDirty
                    then " (dirty)"
                    else ""]|]

-- | Add a command to the options dispatcher.
addCommand :: String   -- ^ command string
           -> String   -- ^ title of command
           -> (a -> b) -- ^ constructor to wrap up command in common data type
           -> Parser a -- ^ command parser
           -> Writer (Mod CommandFields b) ()
addCommand cmd title constr inner =
  tell $
  command cmd
          (info (constr <$> inner)
                (progDesc title))
