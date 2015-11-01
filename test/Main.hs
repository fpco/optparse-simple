{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative.Simple
import GHC.IO.Handle
import System.IO
import System.Environment
import Control.Exception
import Control.Monad
import System.Directory
import System.Exit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS


shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected
  | expected == actual = return ()
  | otherwise = do
    putStrLn $ "expected: " ++ show expected
    putStrLn $ "actual  : " ++ show actual
    exitFailure

catchReturn :: Exception e => IO e -> IO e
catchReturn io = io `catch` return

catchExitCode :: IO () -> IO ExitCode
catchExitCode action = catchReturn $ do
  action
  return ExitSuccess

data FakeHandles = FakeHandles
  { fakeIn  :: Handle
  , fakeOut :: Handle
  , fakeErr :: Handle
  , realIn  :: Handle
  , realOut :: Handle
  , realErr :: Handle
  }

openFile' :: FilePath -> IO Handle
openFile' path = do
  removeIfExists path
  openFile path ReadWriteMode

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ do
    removeFile path

stdinFile :: FilePath
stdinFile = ".tmp.stdin"

stdoutFile :: FilePath
stdoutFile = ".tmp.stdout"

stderrFile :: FilePath
stderrFile = ".tmp.stderr"

beforeFH :: IO FakeHandles
beforeFH = do
  realIn <- hDuplicate stdin
  realOut <- hDuplicate stdout
  realErr <- hDuplicate stderr

  fakeIn <- openFile stdinFile ReadWriteMode
  fakeOut <- openFile' stdoutFile
  fakeErr <- openFile' stderrFile

  hDuplicateTo fakeIn stdin
  hDuplicateTo fakeOut stdout
  hDuplicateTo fakeErr stderr

  return FakeHandles{..}

afterFH :: FakeHandles -> IO ()
afterFH FakeHandles{..} = do
  hDuplicateTo realIn stdin
  hDuplicateTo realOut stdout
  hDuplicateTo realErr stderr

  hClose fakeIn
  hClose fakeOut
  hClose fakeErr

withFakeHandles :: IO a -> IO a
withFakeHandles = bracket beforeFH afterFH . const

withStdIn :: ByteString -> IO ()
  -> IO (ByteString, ByteString, ExitCode)
withStdIn inBS action = do
  BS.writeFile stdinFile inBS
  withFakeHandles $ do
    code <- catchExitCode action
    hFlush stdout
    hFlush stderr
  out <- BS.readFile stdoutFile
  err <- BS.readFile stderrFile

  removeIfExists stdinFile
  removeIfExists stdoutFile
  removeIfExists stderrFile

  return (out, err, ExitSuccess)


main :: IO ()
main = do
  (out, err, exitCode) <- withStdIn ""
    $ withArgs ["--version"]
    $ simpleProg
  exitCode `shouldBe` ExitSuccess
  out `shouldBe` "version\n"

  (out', err', exitCode') <- withStdIn ""
    $ withArgs ["--summary"]
    $ summaryProg
  exitCode' `shouldBe` ExitSuccess
  out' `shouldBe` "A program summary\n"

  return ()


simpleProg :: IO ()
simpleProg = do
  ((), ()) <- simpleOptions "version" "header" "desc" (pure ()) empty
  return ()

parserWithSummary :: Parser ()
parserWithSummary = summaryOption <*> pure () where
  summaryOption = infoOption "A program summary"
    $ long "summary"
   <> help "Show program summary"

summaryProg :: IO ()
summaryProg = do
  ((), ()) <- simpleOptions "version" "header" "desc" parserWithSummary empty
  return ()
