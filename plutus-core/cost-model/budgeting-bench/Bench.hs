{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- See Note [Creation of the Cost Model]
module Main (main) where

import           Language.PlutusCore                                as PLC
import           Language.PlutusCore.Evaluation.Machine.ExBudgeting
import           Language.PlutusCore.Evaluation.Machine.ExMemory
import           Language.PlutusCore.MkPlc
import           Language.UntypedPlutusCore                         as UT
import           Language.UntypedPlutusCore.Evaluation.Machine.Cek

import           Criterion.Main
import qualified Criterion.Types                                    as C
import qualified Data.ByteString                                    as BS
import           Data.Functor
import qualified Data.Kind                                          as GHC
import           Hedgehog
import           Hedgehog.Internal.Gen
import           Hedgehog.Internal.Tree
import           Hedgehog.Range
import           System.Directory
import           System.FilePath
import           System.Random

type UntypedPlain f (uni :: GHC.Type -> GHC.Type) (fun :: GHC.Type) = f Name uni fun ()


-- Not totally sure what's going on here.  `env` is supposed to procude data
-- that will be supplied to the things being benchmarked.  Here we've got a term
-- and we evaluate it to get back the budget consumed, but then we throw that away
-- and evaluate the term again.  This may have the effect of avoiding warmup, but
-- is that really why it's like this?  At least we're only (I think) doing the
-- appparently redundant thing once.
runTermBench :: String -> UntypedPlain UT.Term DefaultUni DefaultFun -> Benchmark
runTermBench name term = env
    (do
        (_result, budget) <-
          pure $ runCekNoEmit defBuiltinsRuntime enormousBudget term
        pure budget
        )
    $ \_ -> bench name $ nf (unsafeEvaluateCek defBuiltinsRuntime) term

-- Copying the bytestring here, because otherwise it'll be exactly the same, and the equality will short-circuit.
benchSameTwoByteStrings :: DefaultFun -> Benchmark
benchSameTwoByteStrings name = createTwoTermBuiltinBench name (byteStringsToBench seedA) ((\(bs, e) -> (BS.copy bs, e)) <$> byteStringsToBench seedA)

benchTwoByteStrings :: DefaultFun -> Benchmark
benchTwoByteStrings name = createTwoTermBuiltinBench name (byteStringsToBench seedA) (byteStringsToBench seedB)

benchBytestringOperations :: DefaultFun -> Benchmark -- TODO the numbers are a bit too big here
benchBytestringOperations name = createTwoTermBuiltinBench @Integer @BS.ByteString name numbers (byteStringsToBench seedA)
    where
        numbers = expToBenchingInteger <$> expsToBench

createTwoTermBuiltinBench
    :: (DefaultUni `Includes` a, DefaultUni `Includes` b)
    => DefaultFun
    -> [(a, ExMemory)]
    -> [(b, ExMemory)]
    -> Benchmark
createTwoTermBuiltinBench name as bs =
    bgroup (show name) $
        as <&> (\(x, xMem) ->
            bgroup (show xMem) $ bs <&> (\(y, yMem) ->
                runTermBench (show yMem) $ erase $
                             mkIterApp () (builtin () name) [mkConstant () x,  mkConstant () y]
            ))

        as

-- TODO: create a similar function for comparisons, but given a list [x1,x2, ...]
-- just do the benchmarking on (x1,x1), (x2,x2), ... since that is the worst case.

benchComparison :: [Benchmark]
benchComparison = (\n -> runTermBench ("CalibratingBench/ExMemory " <> show n) (erase $ createRecursiveTerm n)) <$> [1..20]

-- Creates a cheap builtin operation to measure the base cost of executing one.
createRecursiveTerm :: Integer -> Plain PLC.Term DefaultUni DefaultFun
createRecursiveTerm d = mkIterApp () (builtin () AddInteger)
                        [ (mkConstant () (1::Integer))
                        , if d == 0
                          then mkConstant () (1::Integer)
                          else createRecursiveTerm (d - 1)
                        ]

benchHashOperations :: DefaultFun -> Benchmark
benchHashOperations name =
    bgroup (show name) $
        byteStringsToBench seedA <&> (\(x, xMem) ->
            runTermBench (show xMem) $ erase $ mkIterApp () (builtin () name) [(mkConstant () x)]
        )

-- for VerifySignature, for speed purposes, it shouldn't matter if the sig / pubkey are correct
sig :: BS.ByteString
sig = "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
pubKey :: BS.ByteString
pubKey = "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
benchVerifySignature :: Benchmark
benchVerifySignature =
    bgroup (show name) $
        bs <&> (\(x, xMem) ->
            runTermBench (show xMem) $ erase $
                         mkIterApp () (builtin () name)
                         [ mkConstant () pubKey
                         , mkConstant () x
                         , mkConstant () sig
                         ]
        )
    where
        name = VerifySignature
        bs = (expToBenchingBytestring seedA . fromInteger) <$> expsToBenchBS

expsToBenchBS :: [Integer]
expsToBenchBS = ((\(a :: Integer) -> 2^a) <$> [1..20])

byteStringsToBench :: Seed -> [(BS.ByteString, ExMemory)]
byteStringsToBench seed = (expToBenchingBytestring seed . fromInteger) <$> expsToBenchBS

expsToBench :: [Integer]
expsToBench = ((\(a :: Integer) -> 2^a) <$> [1..16]) -- <> ((\(a :: Integer) -> 10^a) <$> [3..8])

seedA :: Seed
seedA = (Seed 42 43)
seedB :: Seed
seedB = (Seed 44 45)

genSample :: Seed -> Gen a -> a
genSample seed gen = Prelude.maybe (Prelude.error "Couldn't create a sample") treeValue $ evalGen (Size 1) seed gen

-- TODO make a nice class out of these
expToBenchingBytestring :: Seed -> Int -> (BS.ByteString, ExMemory)
expToBenchingBytestring seed e = let x = genSample seed (bytes (Hedgehog.Range.singleton e)) in (x, memoryUsage x)

-- TODO make the e the actual ExMemory size
expToBenchingInteger :: Integer -> (Integer, ExMemory)
expToBenchingInteger e =
    let
        x = (3 :: Integer) ^ e
    in (x, memoryUsage x)


-- Generate a random n-word integer
randNwords :: Integer -> StdGen -> (Integer, StdGen)
randNwords n gen = randomR (lb,ub) gen
    where lb = 2^(64*(n-1))
          ub = 2^(64*n) - 1

-- Given a list [n_1, n_2, ...] create a list [m_1, m_2, ...] where m_i is an n_i-word random integer
makeNumbers :: [Integer] -> StdGen -> ([Integer], StdGen)
makeNumbers [] g     = ([], g)
makeNumbers (n:ns) g =
    let (m,g1) = randNwords n g
        (ms,g2) = makeNumbers ns g1
    in (m:ms,g2)

benchTwoInt :: StdGen -> DefaultFun -> Benchmark
benchTwoInt gen builtinName =
    createTwoTermBuiltinBench builtinName inputs inputs
    where
      (numbers,_) = makeNumbers [1..30] gen
      inputs  = fmap (\e -> (e, memoryUsage e)) numbers

-- Creates the .csv file consumed by create-cost-model. The data in said csv is
-- time taken for all the builtin operations, as measured by criterion.
-- See also Note [Creation of the Cost Model]
--
-- TODO: Some care is required here regarding the current working directory.  If
-- you run this benchmark via `cabal bench` or `stack bench` (but not `cabal
-- run`) then the current directory will be `plutus-core`.  If you use nix it'll
-- be the current shell directory, so you'll need to run it from `plutus-core`
-- (NOT `plutus`, where `default.nix` is).  See SCP-2005.
main :: IO ()
main = do
  gen <- getStdGen
  let dataDir = "cost-model" </> "data"
      csvFile = dataDir </> "benching.csv"
      backupFile = dataDir </> "benching.csv.backup"
  createDirectoryIfMissing True dataDir
  csvExists <- doesFileExist csvFile
  if csvExists then renameFile csvFile backupFile else pure ()

  defaultMainWith (defaultConfig { C.csvFile = Just csvFile })
                        $  (benchTwoInt gen <$> twoIntNames)
                        <> (benchTwoByteStrings <$> [Concatenate])
                        <> (benchBytestringOperations <$> [DropByteString, TakeByteString])
                        <> (benchHashOperations <$> [SHA2, SHA3])
                        <> (benchSameTwoByteStrings <$> [EqByteString, LtByteString, GtByteString])
                        <> [benchVerifySignature]
                        <> benchComparison

  pure ()
    where twoIntNames = [ AddInteger, SubtractInteger
                        , MultiplyInteger
                        , QuotientInteger, RemainderInteger
                        , DivideInteger, ModInteger
                        , LessThanInteger, LessThanEqInteger
                        , GreaterThanInteger, GreaterThanEqInteger
                        , EqInteger
                        ]
