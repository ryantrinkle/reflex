{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:
--   Reflex.Optimizer
-- Description:
--   This module provides a GHC plugin designed to improve code that uses
--   Reflex.  Currently, it just adds an INLINABLE pragma to any top-level
--   definition that doesn't have an explicit inlining pragma.  In the future,
--   additional optimizations are likely to be added.
module Reflex.Optimizer
  ( plugin
  ) where

#ifdef ghcjs_HOST_OS
import Plugins
#else
import Control.Arrow
import CoreMonad
import Data.String
import GhcPlugins

import Prelude hiding ((<>))

#endif

#ifdef ghcjs_HOST_OS

-- | The GHCJS build of Reflex.Optimizer just throws an error; instead, the version built with GHC should be used.
plugin :: Plugin
plugin = error "The GHCJS build of Reflex.Optimizer cannot be used.  Instead, build with GHC and use the result with GHCJS."

#else

-- | The GHC plugin itself.  See "GhcPlugins" for more details.
plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [] p = do
  liftIO $ putStrLn $ showSDocUnsafe $ ppr p
  let f = \case
        simpl@(CoreDoSimplify _ _) -> [CoreDoSpecialising, simpl]
        x -> [x]
  return $ makeInlinable : concatMap f p
install options@(_:_) p = do
  warnMsg $ "Reflex.Optimizer: ignoring " <> fromString (show $ length options) <> " command-line options"
  install [] p

makeInlinable :: CoreToDo
makeInlinable = CoreDoPluginPass "MakeInlinable" $ \modGuts -> do
  let f v = setIdInfo v $ let i = idInfo v in
        setInlinePragInfo i $ let p = inlinePragInfo i in
        if isDefaultInlinePragma p
        then defaultInlinePragma { inl_inline = Inlinable }
        else p
      newBinds = flip map (mg_binds modGuts) $ \case
        NonRec b e -> NonRec (f b) e
        Rec bes -> Rec $ map (first f) bes
  return $ modGuts { mg_binds = newBinds }

#endif
