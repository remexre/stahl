module Commands.Check (subcommand) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (getContents)
import Language.Stahl (Error, parse, parseFile)
import Language.Stahl.Util (printError, writeOrStdout)
import Options.Applicative
import Prelude hiding (getContents)

subcommand :: Mod CommandFields (IO ())
subcommand = command "check" (info parser (progDesc "Checks a file for errors."))
  where parser = run
         <$> strArgument
             ( help "The file to read from."
            <> metavar "INPUT-PATH"
            <> showDefault
            <> value "-"
             )

run :: FilePath -> IO ()
run path = runExceptT (run' path) >>= \case
  Left e -> printError $ show e
  Right () -> pure ()

run' :: FilePath -> ExceptT Error IO ()
run' path = do
  vals <- parseFile path
  error ("TODO: " <> show vals)
