{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
import           Data.Maybe
import           Data.Version
import           Development.GitRev (gitDirty, gitHash)
import           Language.Haskell.TH (Q,Exp)
import qualified Language.Haskell.TH.Syntax as TH
import           Options.Applicative
import           Options.Applicative.Types
import           System.Environment
import           Debug.Trace
import           System.IO.Unsafe
import           Data.Tree
import qualified Data.List as L

-- -- | Generate and execute a simple options parser.
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

class Monoid outer => HasOuter inner outer | inner -> outer where
  getOuter :: inner -> outer
  applyInnerToOuter :: inner -> outer -> outer

simpleOptions'
  :: (Show a, Monoid a)
  => String
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
simpleOptions' versionString h pd globalParser commandParser = do
    args <- getArgs
    (outer, inner) <- go args
    return (outer, inner)
  where
    parser =
        info
            (versionOption <*>
             simpleParser' globalParser commandParser)
            desc
    desc =
        fullDesc <> header h <> progDesc pd
    versionOption =
        infoOption
            versionString
            (long "version" <>
             help "Show version")
    execP =
        execParserPure'
            (prefs idm)
            parser

    go args' =
        case execP args' of
            (Failure _, _)
                | null args' ->
                    withArgs
                        ["--help"]
                        (execParser parser)
            (parseResult, _) -> do
                handleParseResult parseResult

swapLastTwo :: [a] -> [a]
swapLastTwo (reverse -> (ul:pen:xs)) = reverse (pen:ul:xs)
swapLastTwo x = x

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
addCommand :: (HasOuter a b)
           => String   -- ^ command string
           -> String   -- ^ title of command
           -> (a -> b -> c) -- ^ constructor to wrap up command in common data type
           -> Parser a -- ^ command parser
           -> EitherT (b -> c) (Writer (Mod CommandFields (b -> c))) ()
addCommand cmd title constr inner =
  lift (tell (command cmd
                      (info (foo constr <$> inner)
                            (progDesc title))))


foo :: HasOuter a b => Monoid b => (a -> b -> c) -> a -> (b -> c)
foo f x =
  let ab = getOuter x
  in (\x' -> f x (ab <> x'))
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
-- addSubCommands
--   :: String
--   -- ^ command string
--   -> String
--   -- ^ title of command
--   -> EitherT b (Writer (Mod CommandFields b)) ()
--   -- ^ sub-commands (use 'addCommand')
--   -> EitherT b (Writer (Mod CommandFields b)) ()
-- addSubCommands cmd title commandParser =
--   addCommand cmd
--              title
--              (\((), a) -> a)
--              (simpleParser (pure ()) commandParser)

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
  -> EitherT b (Writer (Mod CommandFields b)) ()
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
          case runWriter (runEitherT commandParser) of
            (Right (),d) -> subparser d
            (Left b,_) -> pure b

data InnerOuterParser a b = Outer a b 

class BuiltParser a b c | a b -> c where
  buildParser :: Parser a -> Parser b -> Parser c
  unwrap :: c -> (a,b)

instance (Show a, Monoid a) => BuiltParser a b (InnerOuterParser a b) where
  buildParser outer inner = Outer <$> outer <*> inner
  unwrap (Outer a b) = (a,b)

constructParser
  :: (Show a, Monoid a)
  => BuiltParser a b c => Parser a
  -- ^ common settings
  -> Parser b
  -- ^ commands (use 'addCommand')
  -> Parser (InnerOuterParser a b)
constructParser = buildParser

simpleParser'
  :: (Show a, Monoid a)
  => Parser a
  -- ^ common settings
  -> EitherT b (Writer (Mod CommandFields b)) ()
  -- ^ commands (use 'addCommand')
  -> Parser (a, b)
simpleParser' commonParser commandParser =
  helpOption <*> config
  where helpOption =
          abortOption ShowHelpText $
          long "help" <>
          help "Show this help text"
        config =
          (,) <$> commonParser <*>
          case runWriter (runEitherT commandParser) of
            (Right (),d) -> subparser d
            (Left b,_) -> pure b
