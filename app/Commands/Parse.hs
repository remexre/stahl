module Commands.Parse (subcommand) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Language.Stahl (Error, parseFile)
import Language.Stahl.Util (printError, writeOrStdout)
import Options.Applicative

subcommand :: Mod CommandFields (IO ())
subcommand = command "parse" (info parser (progDesc "Reads a program and prints it as s-expressions."))
  where parser = run
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

run :: FilePath -> FilePath -> IO ()
run inputPath outputPath = runExceptT (run' inputPath outputPath) >>= \case
  Left e -> printError $ show e
  Right () -> pure ()

run' :: FilePath -> FilePath -> ExceptT Error IO ()
run' inputPath outputPath = liftIO . writeOrStdout outputPath . unlines . map show =<< parseFile inputPath
