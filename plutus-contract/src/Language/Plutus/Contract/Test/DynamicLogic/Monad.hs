{-# LANGUAGE FlexibleContexts #-}
module Language.Plutus.Contract.Test.DynamicLogic.Monad
    ( DL
    , action
    , anyAction
    , anyActions
    , anyActions_
    , stopping
    , weight
    , getModelStateDL
    , assert
    , assertModel
    , forAllQ
    , forAllDL
    , DL.DynLogic
    , DL.DynLogicModel(..)
    , module Language.Plutus.Contract.Test.DynamicLogic.Quantify
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Typeable

import qualified Language.Plutus.Contract.Test.DynamicLogic          as DL
import           Language.Plutus.Contract.Test.DynamicLogic.Quantify
import           Language.Plutus.Contract.Test.StateModel

import           Test.QuickCheck

newtype DL s a = DL { unDL :: s -> (a -> DL.DynPred s) -> DL.DynLogic s }
    deriving (Functor)

instance Applicative (DL s) where
    pure x = DL $ \ s k -> k x s
    (<*>)  = ap

instance Monad (DL s) where
    return = pure
    DL h >>= j = DL $ \ s k -> h s $ \ x s1 -> unDL (j x) s1 k

action :: (Show a, Typeable a, Eq (Action s a)) => Action s a -> DL s ()
action cmd = DL $ \ _ k -> DL.after cmd $ k ()

anyAction :: DL s ()
anyAction = DL $ \ _ k -> DL.afterAny $ k ()

anyActions :: Int -> DL s ()
anyActions n = stopping <|> weight (1 / fromIntegral n)
                        <|> (anyAction >> anyActions n)

anyActions_ :: DL s ()
anyActions_ = stopping <|> (anyAction >> anyActions_)

stopping :: DL s ()
stopping = DL $ \ s k -> DL.toStop (k () s)

weight :: Double -> DL s ()
weight w = DL $ \ s k -> DL.weight w (k () s)

-- TODO: better name!
getModelStateDL :: DL s s
getModelStateDL = DL $ \ s k -> k s s

errorDL :: String -> DL s a
errorDL name = DL $ \ _ _ -> DL.errorDL name

assert :: String -> Bool -> DL s ()
assert name b = if b then return () else errorDL name

assertModel :: String -> (s -> Bool) -> DL s ()
assertModel name p = assert name . p =<< getModelStateDL

forAllQ :: Quantifiable q => q -> DL s (Quantifies q)
forAllQ q = DL $ \ s k -> DL.forAllQ q $ \ x -> k x s

instance Alternative (DL s) where
    empty = DL $ \ _ _ -> DL.ignore
    DL h <|> DL j = DL $ \ s k -> h s k DL.||| j s k

runDL :: s -> DL s () -> DL.DynLogic s
runDL s dl = unDL dl s $ \ _ _ -> DL.passTest

forAllDL :: (DL.DynLogicModel s, Testable a) =>
            DL s () -> (Script s -> a) -> Property
forAllDL d prop = DL.forAllScripts (runDL initialState d) prop

