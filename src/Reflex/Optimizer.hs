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

-- | The GHCJS build of Reflex.Optimizer just throws an error; instead, the version built with GHC should be used.
plugin :: Plugin
plugin = error "The GHCJS build of Reflex.Optimizer cannot be used.  Instead, build with GHC and use the result with GHCJS."

#else

#if MIN_VERSION_base(4,9,0)
import Prelude hiding ((<>))
#endif

import Control.Arrow
import Data.String

#if MIN_VERSION_base(4,15,0)
import GHC.Core.Opt.Monad
import GHC.Core.Opt.Pipeline.Types
import GHC.Plugins
import GHC.Types.Error

-- | The GHC plugin itself.  See "GHC.Plugins" for more details.
plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [] p = do
  liftIO $ putStrLn $ showSDocUnsafe $ ppr p
  let f = \case
        simpl@(CoreDoSimplify _) -> [CoreDoSpecialising, simpl]
        x -> [x]
  return $ makeInlinable : concatMap f p
install options@(_:_) p = do
  msg MCInfo $ "Reflex.Optimizer: ignoring " <> fromString (show $ length options) <> " command-line options"
  install [] p

makeInlinable :: CoreToDo
makeInlinable = CoreDoPluginPass "MakeInlinable" $ \modGuts -> do
  let f v = setIdInfo v $ let i = idInfo v in
        setInlinePragInfo i $ let p = inlinePragInfo i in
        if isDefaultInlinePragma p
        then defaultInlinePragma { inl_inline = Inlinable (inl_src p) }
        else p
      newBinds = flip map (mg_binds modGuts) $ \case
        NonRec b e -> NonRec (f b) e
        Rec bes -> Rec $ map (first f) bes
  return $ modGuts { mg_binds = newBinds }
#else
import CoreMonad
import GhcPlugins

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

#endif
