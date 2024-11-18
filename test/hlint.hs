module Main where

import Control.Monad
import Language.Haskell.HLint (hlint)
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.FilePath.Find

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  let runHlint f = hlint $ f:
        [ "--ignore=Redundant do"
        , "--ignore=Use camelCase"
        , "--ignore=Redundant $"
        , "--ignore=Use &&"
        , "--ignore=Use &&&"
        , "--ignore=Use const"
        , "--ignore=Use >=>"
        , "--ignore=Use ."
        , "--ignore=Use unless"
        , "--ignore=Reduce duplication"
        , "--ignore=Replace case with maybe"
        , "--cpp-define=DEBUG"
        , "--ignore=Use tuple-section"
        ]
      recurseInto = and <$> sequence
        [ fileType ==? Directory
        , fileName /=? ".git"
        ]
      matchFile = and <$> sequence
        [ extension ==? ".hs"
        ]
  files <- find recurseInto matchFile (pwd </> "src") --TODO: Someday fix all hints in tests, etc.
  ideas <- fmap concat $ forM files $ \f -> do
    putStr $ "linting file " ++ drop (length pwd + 1) f ++ "... "
    runHlint f
  if null ideas then exitSuccess else exitFailure
