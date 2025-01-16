{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
-- | Template Haskell helper functions for building complex 'Dynamic' values.
module Reflex.Dynamic.TH
  ( qDynPure
  , unqDyn
  , mkDynPure
  ) where

import Reflex.Dynamic

import Control.Monad.State
import Data.Data
import Data.Generics
import qualified Language.Haskell.Exts as Hs
import qualified Language.Haskell.Meta.Syntax.Translate as Hs
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH

-- | Quote a 'Dynamic' expression.  Within the quoted expression, you can use
-- @$(unqDyn [| x |])@ to refer to any expression @x@ of type @Dynamic t a@; the
-- unquoted result will be of type @a@
qDynPure :: Q Exp -> Q Exp
qDynPure qe = do
  e <- qe
  let f :: forall d. Data d => d -> StateT [(Name, Exp)] Q d
      f d = case eqT of
        Just (Refl :: d :~: Exp)
          | AppE (VarE m) eInner <- d
          , m == 'unqMarker
          -> do n <- lift $ newName "dynamicQuotedExpressionVariable"
                modify ((n, eInner):)
                return $ VarE n
        _ -> gmapM f d
  (e', exprsReversed) <- runStateT (gmapM f e) []
  let exprs = reverse exprsReversed
      arg = foldr
        (\(_, expr) rest -> [e| FHCons $(pure expr) $rest |])
        [e| FHNil |]
        exprs
      param = foldr
        (\(name, _) rest -> [p| HCons $(pure $ VarP name) $rest |])
        [p| HNil |]
        exprs
  [| (\ $param -> $(pure e')) <$> distributeFHListOverDynPure $arg |]

-- | Antiquote a 'Dynamic' expression.  This can /only/ be used inside of a
-- 'qDyn' quotation.
unqDyn :: Q Exp -> Q Exp
unqDyn e = [| unqMarker $e |]

-- | This type represents an occurrence of unqDyn before it has been processed
-- by qDyn.  If you see it in a type error, it probably means that unqDyn has
-- been used outside of a qDyn context.
data UnqDyn

-- unqMarker must not be exported; it is used only as a way of smuggling data
-- from unqDyn to qDyn

--TODO: It would be much nicer if the TH AST was extensible to support this kind of thing without trickery
unqMarker :: a -> UnqDyn
unqMarker = error "An unqDyn expression was used outside of a qDyn expression"

-- | Create a 'Dynamic' value using other 'Dynamic's as inputs.  The result is
-- sometimes more concise and readable than the equivalent 'Applicative'-based
-- expression.  For example:
--
-- > [mkDyn| $x + $v * $t + 1/2 * $a * $t ^ 2 |]
--
-- would have a very cumbersome 'Applicative' encoding.
mkDynPure :: QuasiQuoter
mkDynPure = QuasiQuoter
  { quoteExp = mkDynExp
  , quotePat = error "mkDyn: pattern splices are not supported"
  , quoteType = error "mkDyn: type splices are not supported"
  , quoteDec = error "mkDyn: declaration splices are not supported"
  }

mkDynExp :: String -> Q Exp
mkDynExp s = case Hs.parseExpWithMode Hs.defaultParseMode { Hs.extensions = [ Hs.EnableExtension Hs.TemplateHaskell ] } s of
  Hs.ParseFailed (Hs.SrcLoc _ l c) err -> fail $ "mkDyn:" <> show l <> ":" <> show c <> ": " <> err
  Hs.ParseOk e -> qDynPure $ return $ everywhere (id `extT` reinstateUnqDyn) $ Hs.toExp $ everywhere (id `extT` antiE) e
    where TH.Name (TH.OccName occName) (TH.NameG _ _ (TH.ModName modName)) = 'unqMarker
          antiE :: Hs.Exp Hs.SrcSpanInfo -> Hs.Exp Hs.SrcSpanInfo
          antiE x = case x of
            Hs.SpliceExp l se ->
              Hs.App l (Hs.Var l $ Hs.Qual l (Hs.ModuleName l modName) (Hs.Ident l occName)) $ case se of
                Hs.IdSplice l2 v -> Hs.Var l2 $ Hs.UnQual l2 $ Hs.Ident l2 v
                Hs.ParenSplice _ ps -> ps
            _ -> x
          reinstateUnqDyn (TH.Name (TH.OccName occName') (TH.NameQ (TH.ModName modName')))
            | modName == modName' && occName == occName' = 'unqMarker
          reinstateUnqDyn x = x
