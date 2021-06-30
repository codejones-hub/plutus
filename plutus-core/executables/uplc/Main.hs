{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module Main (main) where


import           Common
import qualified PlutusCore                               as PLC

import qualified UntypedPlutusCore                        as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import           Data.Function                            ((&))
import           Data.Functor                             (void)
import           Data.List                                (nub)

import           Control.DeepSeq                          (rnf)
import           Options.Applicative                      (ParserInfo, customExecParser, prefs, showHelpOnEmpty)
import           System.Exit                              (exitFailure, exitSuccess)

uplcHelpText :: String
uplcHelpText = helpText "Untyped Plutus Core"

uplcInfoCommand :: ParserInfo Command
uplcInfoCommand = plutus "Untyped Plutus Core Tool" uplcHelpText

---------------- Script application ----------------

-- | Apply one script to a list of others.
runApply :: ApplyOptions -> IO ()
runApply (ApplyOptions inputfiles ifmt outp ofmt mode) = do
  scripts <- mapM ((getProgram ifmt ::  Input -> IO (UplcProg PLC.AlexPosn)) . FileInput) inputfiles
  let appliedScript =
        case map (\case p -> () <$ p) scripts of
          []          -> errorWithoutStackTrace "No input files"
          progAndargs -> foldl1 UPLC.applyProgram progAndargs
  writeProgram outp ofmt mode appliedScript


runEval :: EvalOptions -> IO ()
runEval (EvalOptions inp ifmt evalMode printMode budgetMode timingMode cekModel) =
    case evalMode of
                CK  -> errorWithoutStackTrace "There is no CK machine for Untyped Plutus Core"
                CEK -> do
                    prog <- getProgram ifmt inp
                    let term = void . UPLC.toTerm $ prog
                        !_ = rnf term
                        cekparams = case cekModel of
                                    Default -> PLC.defaultCekParameters  -- AST nodes are charged according to the default cost model
                                    Unit    -> PLC.unitCekParameters     -- AST nodes are charged one unit each, so we can see how many times each node
                                                                        -- type is encountered.  This is useful for calibrating the budgeting code.
                    case budgetMode of
                        Silent -> do
                            let evaluate = Cek.evaluateCekNoEmit cekparams
                            case timingMode of
                                NoTiming -> evaluate term & handleEResult printMode
                                Timing n -> timeEval n evaluate term >>= handleTimingResults term
                        Verbose bm -> do
                            let evaluate = Cek.runCekNoEmit cekparams bm
                            case timingMode of
                                NoTiming -> do
                                        let (result, budget) = evaluate term
                                        printBudgetState term cekModel budget
                                        handleResultSilently result  -- We just want to see the budget information
                                Timing n -> timeEval n evaluate term >>= handleTimingResultsWithBudget term
    where handleResultSilently = \case
                Right _  -> exitSuccess
                Left err -> print err >> exitFailure
          handleTimingResultsWithBudget term results =
                case nub results of
                [(Right _, budget)] -> do
                    putStrLn ""
                    printBudgetState term cekModel budget
                    exitSuccess
                [(Left err,   budget)] -> do
                    putStrLn ""
                    print err
                    printBudgetState term cekModel budget
                    exitFailure
                _                                   -> error "Timing evaluations returned inconsistent results"

----------------- Print examples -----------------------
runUplcPrintExample ::
    ExampleOptions -> IO ()
runUplcPrintExample = runPrintExample getUplcExamples

---------------- Parse and print a UPLC source file ----------------

runPrint :: PrintOptions -> IO ()
runPrint (PrintOptions inp mode) =
    (parseInput inp :: IO (UplcProg PLC.AlexPosn)) >>= print . getPrintMethod mode

---------------- Conversions ----------------

-- | Convert between textual and CBOR representations.  This subsumes the
-- `print` command: for example, `plc convert -i prog.plc --typed --fmt Readable`
-- will read a typed plc file and print it in the Readable format.  Having
-- the separate `print` option may be more user-friendly though.
runConvert :: ConvertOptions -> IO ()
runConvert (ConvertOptions inp ifmt outp ofmt mode) = do
    program <- (getProgram ifmt inp :: IO (UplcProg PLC.AlexPosn))
    writeProgram outp ofmt mode program


main :: IO ()
main = do
    options <- customExecParser (prefs showHelpOnEmpty) uplcInfoCommand
    case options of
        Apply     opts -> runApply        opts
        Typecheck _    -> errorWithoutStackTrace "Not supported in Untyped plutus core."
        Eval      opts -> runEval         opts
        Example   opts -> runUplcPrintExample opts
        Erase     _    -> errorWithoutStackTrace "Not supported in Untyped plutus core."
        Print     opts -> runPrint        opts
        Convert   opts -> runConvert      opts