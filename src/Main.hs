module Main
  ( main
  ) where

import Options.Applicative

main :: IO ()
main = execParser optParser >>= run

run :: Options -> IO ()
run (ParseToSExprs inputPath outputPath) = do
  undefined

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
          ( command "parse-to-sexprs"
            (info parseToSexprs
             (progDesc "Reads a program and prints it as s-expressions."))
          )
        parseToSexprs = ParseToSExprs
         <$> strOption
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
