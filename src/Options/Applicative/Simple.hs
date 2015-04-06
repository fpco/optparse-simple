{-# LANGUAGE TemplateHaskell #-}

-- | Simple interface to program arguments.
--
-- Typical usage with no commands:
--
-- @
-- do (opts,()) <-
--      simpleOptions "ver"
--                    "header"
--                    "desc"
--                    (flag () () (long "some-flag"))
--                    empty
--    doThings opts
-- @
--
-- Typical usage with commands:
--
-- @
-- do (opts,runCmd) <-
--      simpleOptions "ver"
--                    "header"
--                    "desc"
--                    (pure ()) $
--      do addCommand "delete"
--                    "Delete the thing"
--                    (const deleteTheThing)
--                    (pure ())
--         addCommand "create"
--                    "Create a thing"
--                    createAThing
--                    (strOption (long "hello"))
--    runCmd
-- @

module Options.Applicative.Simple
  ( module Options.Applicative.Simple
  , module Options.Applicative
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Writer
import           Data.Monoid
import           Data.Version
import           Development.GitRev (gitDirty, gitHash)
import           Language.Haskell.TH (Q,Exp)
import qualified Language.Haskell.TH.Syntax as TH
import           Options.Applicative

-- | Generate a simple options parser.
simpleOptions
  :: String
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> Parser a
  -- ^ global settings
  -> EitherT b (Writer (Mod CommandFields b)) ()
  -- ^ commands (use 'addCommand')
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
          case (runWriter (runEitherT commandParser)) of
            (Right (),d) -> subparser d
            (Left b,_) -> pure b

-- | Generate a string like @Version 1.2, Git revision 1234@.
--
-- @$(simpleVersion …)@ @::@ 'String'
simpleVersion :: Version -> Q Exp
simpleVersion version =
  [|concat ["Version "
           ,$(TH.lift $ showVersion version)
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
           -> EitherT b (Writer (Mod CommandFields b)) ()
addCommand cmd title constr inner =
  lift (tell (command cmd
                      (info (constr <$> inner)
                            (progDesc title))))
