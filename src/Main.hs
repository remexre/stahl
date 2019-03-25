module Main (main) where

import Control.Exception (ErrorCall(..), SomeException(..), try)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (getContents)
import Data.ByteString.UTF8 (fromString)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Foldable (mapM_)
import Language.Stahl
import Prelude hiding (getContents)
import Options.Applicative
import System.Console.ANSI
  ( Color(Red)
  , ColorIntensity(Vivid)
  , ConsoleLayer(Foreground)
  , SGR(Reset, SetColor)
  , hSetSGR
  , hSupportsANSIColor
  )
import System.IO (hFlush, hPutStr, stderr, stdout)

main :: IO ()
main = execParser optParser >>= run

run :: Options -> IO ()
run opts = try (runExceptT (run' opts)) >>= \case
  Right (Right ()) -> pure ()
  Right (Left err) -> printError $ show err
  Left (SomeException err) ->
    let msg = case fromDynamic (toDyn err) of
          Just (ErrorCallWithLocation msg' loc) -> msg' <> "\nAt location:\n" <> loc
          Nothing -> show err
    in printError ("\n=== INTERNAL COMPILER ERROR ===\n\n" <> msg <> "\n\n--- THIS IS A COMPILER BUG! ---")

run' :: Options -> ExceptT Error IO ()
run' (ParseToSExprs inputPath outputPath) = do
  vals <- if inputPath == "-"
          then parse "<stdin>" =<< liftIO getContents
          else parseFile inputPath
  let print = if outputPath == "-"
              then liftIO . putStr
              else writeFile outputPath
  liftIO . print . unlines $ map show vals

printError :: String -> IO ()
printError err = do
    hFlush stdout
    hFlush stderr
    isatty <- hSupportsANSIColor stderr
    if' isatty $ hSetSGR stderr [SetColor Foreground Vivid Red]
    hPutStr stderr err
    if' isatty $ hSetSGR stderr [Reset]
    hPutStr stderr "\n"
  where if' True b = b
        if' False _ = pure ()

data Options =
  ParseToSExprs
    { inputPath :: FilePath
    , outputPath :: FilePath
    }
  deriving Show

optParser :: ParserInfo Options
optParser = info (parser <**> helper)
    ( fullDesc
   <> header "stahl - A dependently typed Lisp with algebraic effects."
    )
  where parser = subparser
          ( command "parse"
            (info parseToSexprs
             (progDesc "Reads a program and prints it as s-expressions."))
          )
        parseToSexprs = ParseToSExprs
         <$> strArgument
             ( help "The file to read from."
            <> metavar "INPUT-PATH"
            <> showDefault
            <> value "-"
             )
         <*> strOption
             ( help "The file to write to."
            <> long "output"
            <> metavar "OUTPUT-PATH"
            <> short 'o'
            <> showDefault
            <> value "-"
             )
