{-# LANGUAGE LambdaCase #-}
{-|
A simple beta-reduction pass.

-}
module PlutusIR.Transform.Beta
    ( beta
    , betaM
    ) where

import           PlutusPrelude

import           PlutusIR

import           Control.Lens        (transformMOf)
import           Control.Monad.State

{-|
A single non-recursive application of the beta rule.

-}
betaStep
    :: MonadState Bool m => Term tyname name uni fun a
    -> m (Term tyname name uni fun a)
betaStep = \case
    Apply a (LamAbs _ name typ body) arg -> do
        let varDecl  = VarDecl a name typ
            binding  = TermBind a Strict varDecl arg
            bindings = binding :| []
        markDirty
        pure $ Let a NonRec bindings body
    TyInst a (TyAbs _ tyname kind body) typ -> do
        let tyVarDecl = TyVarDecl a tyname kind
            tyBinding = TypeBind a tyVarDecl typ
            bindings  = tyBinding :| []
        markDirty
        pure $ Let a NonRec bindings body
    t -> pure t
    where
        -- Record wether a modification to the AST has occurred.
        markDirty :: MonadState Bool m => m ()
        markDirty = put True

{-|
Recursively apply the beta transformation on the code, both for the terms

@
    (\ (x : A). M) N
    ==>
    let x : A = N in M
@

and types

@
    (/\ a. \(x : a) . x) {A}
    ==>
    let a : * = A in
    (\ (x : A). x)
@

-}

beta
    :: Term tyname name uni fun a
    -> Term tyname name uni fun a
beta = flip evalState False . betaM

betaM
    :: MonadState Bool m => Term tyname name uni fun a
    -> m (Term tyname name uni fun a)
betaM = transformMOf termSubterms betaStep
