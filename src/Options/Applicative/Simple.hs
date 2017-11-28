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
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Writer
import           Data.Monoid
import           Data.Version
import           Development.GitRev (gitDirty, gitHash)
import           Language.Haskell.TH (Q,Exp)
import qualified Language.Haskell.TH.Syntax as TH
import           Options.Applicative
import           System.Environment

-- | Generate and execute a simple options parser.
simpleOptions
  :: String
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> Parser a
  -- ^ global settings
  -> ExceptT b (Writer (Mod CommandFields b)) ()
  -- ^ commands (use 'addCommand')
  -> IO (a,b)
simpleOptions versionString h pd globalParser commandParser =
  do args <- getArgs
     case execParserPure (prefs idm) parser args of
       Failure _ | null args -> withArgs ["--help"] (execParser parser)
       parseResult -> handleParseResult parseResult
  where parser = info (versionOption <*> simpleParser globalParser commandParser) desc
        desc = fullDesc <> header h <> progDesc pd
        versionOption =
          infoOption
            versionString
            (long "version" <>
             help "Show version")

-- | Generate a string like @Version 1.2, Git revision 1234@.
--
-- @$(simpleVersion …)@ @::@ 'String'
simpleVersion :: Version -> Q Exp
simpleVersion version =
  [|concat (["Version "
           ,$(TH.lift $ showVersion version)
           ] ++
           if $gitHash == ("UNKNOWN" :: String)
             then []
             else
               [", Git revision "
               ,$gitHash
               ,if $gitDirty
                   then " (dirty)"
                   else ""
               ])|]

-- | Add a command to the options dispatcher.
addCommand :: String   -- ^ command string
           -> String   -- ^ title of command
           -> (a -> b) -- ^ constructor to wrap up command in common data type
           -> Parser a -- ^ command parser
           -> ExceptT b (Writer (Mod CommandFields b)) ()
addCommand cmd title constr inner =
  lift (tell (command cmd
                      (info (constr <$> (helper <*> inner))
                            (progDesc title))))

-- | Add a command that takes sub-commands to the options dispatcher.
--
-- Example:
--
-- @
-- addSubCommands "thing"
--                "Subcommands that operate on things"
--                (do addCommand "delete"
--                               "Delete the thing"
--                               (const deleteTheThing)
--                               (pure ())
--                    addCommand "create"
--                               "Create a thing"
--                               createAThing
--                               (strOption (long "hello")))
-- @
--
-- If there are common options between all the sub-commands, use 'addCommand'
-- in combination with 'simpleParser' instead of 'addSubCommands'.
addSubCommands
  :: String
  -- ^ command string
  -> String
  -- ^ title of command
  -> ExceptT b (Writer (Mod CommandFields b)) ()
  -- ^ sub-commands (use 'addCommand')
  -> ExceptT b (Writer (Mod CommandFields b)) ()
addSubCommands cmd title commandParser =
  addCommand cmd
             title
             (\((), a) -> a)
             (simpleParser (pure ()) commandParser)

-- | Generate a simple options parser.
--
-- Most of the time you should use 'simpleOptions' instead, but 'simpleParser'
-- can be used for sub-commands that need common options.  For example:
--
-- @
-- addCommand "thing"
--            "Subcommands that operate on things"
--            (\\(opts,runSubCmd) -> runSubCmd opts)
--            (simpleParser (flag () () (long "some-flag")) $
--             do addCommand "delete"
--                           "Delete the thing"
--                           (const deleteTheThing)
--                           (pure ())
--                addCommand "create"
--                           "Create a thing"
--                           createAThing
--                           (strOption (long "hello")))
-- @
--
simpleParser
  :: Parser a
  -- ^ common settings
  -> ExceptT b (Writer (Mod CommandFields b)) ()
  -- ^ commands (use 'addCommand')
  -> Parser (a,b)
simpleParser commonParser commandParser =
  helpOption <*> config
  where helpOption =
          abortOption ShowHelpText $
          long "help" <>
          help "Show this help text"
        config =
          (,) <$> commonParser <*>
          case runWriter (runExceptT commandParser) of
            (Right (),d) -> subparser d
            (Left b,_) -> pure b
