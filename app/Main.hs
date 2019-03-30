module Main (main) where

import qualified Commands.Check
import qualified Commands.Parse
import Control.Exception (ErrorCall(..), SomeException(..), try)
import Data.Dynamic (fromDynamic, toDyn)
import Language.Stahl.Util (printError)
import Options.Applicative

main :: IO ()
main = execParser parser >>= try >>= \case
    Right () -> pure ()
    Left (SomeException err) ->
      let msg = case fromDynamic (toDyn err) of
            Just (ErrorCallWithLocation msg' loc) -> msg' <> "\nAt location:\n" <> loc
            Nothing -> show err
      in printError ("\n=== INTERNAL COMPILER ERROR ===\n\n" <> msg <> "\n\n--- THIS IS A COMPILER BUG! ---")
  where parser = info (subparser (mconcat commands) <**> helper)
                   (fullDesc <> header "stahl - A dependently typed Lisp with algebraic effects.")
        commands = [ Commands.Check.subcommand
                   , Commands.Parse.subcommand
                   ]
