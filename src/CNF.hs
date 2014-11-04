module CNF (Lit (..), Clause, CNF.CNF, lit, notlit, CNF.or, print_dimacs, solve, is_negation) where

import qualified System.Process
import qualified System.IO
import qualified System.Exit
import qualified System.Directory

import qualified Data.Time.Clock
import qualified Data.List

import Debug.Trace (trace)
import qualified Control.DeepSeq
import Control.DeepSeq (($!!))

import qualified Support

minisat_path :: FilePath
minisat_path = "minisat"

data Lit = Yes Int | No Int deriving (Show, Read, Eq)
type Clause = [Lit]
type CNF = [Clause]

instance Control.DeepSeq.NFData Lit where
  rnf (Yes x) = Control.DeepSeq.deepseq x ()
  rnf (No x) = Control.DeepSeq.deepseq x ()

lit :: Int -> Lit
lit = Yes

notlit :: Int -> Lit
notlit = No

or :: [Lit] -> Clause
or = id

is_negation :: Lit -> Bool
is_negation (No _) = True
is_negation (Yes _) = False

print_dimacs :: CNF -> String
print_dimacs cnf =
  let
    mapmap f = map (map f)
    get_var l = case l of Yes x -> x; No x -> x
    nvars = maximum $ map maximum $ mapmap get_var cnf
    neqs = length cnf
    heading = "c Generated by CNF module\np cnf " ++ show nvars ++ " " ++ show neqs ++ "\n"
    print_lit l = case l of Yes x -> show x; No x -> "-" ++ show x
    print_clause cl = unwords (map print_lit cl) ++ " 0\n"
  in
    heading ++ concatMap print_clause cnf

parse_minisat_assigns [] = Left "error in .cnf file: missing variable assignments"
parse_minisat_assigns (s:_)
    | last nums /= 0 = Left "error in .cnf file: trailing zero missing"
    | elem 0 nums_sans_last = Left "error in .cnf file: spurious zeros"
    | any (\(a, i) -> abs a /= i) $ zip nums_sans_last [1 ..] = Left "error in .cnf file: invalid variable ordering"
    | otherwise = Right $ Just $ map int_to_lit nums_sans_last
  where
    nums = map read $ Support.deintersperse ' ' s
    nums_sans_last = Support.drop_last nums
    int_to_lit x = if x < 0 then No (-x) else Yes x

--parse_dimacs_status ("SAT":rest) = let x = parse_dimacs_assigns rest in trace (show x) $! x
parse_minisat_status ("SAT":rest) = parse_minisat_assigns rest
parse_minisat_status ("UNSAT":_) = Right Nothing
parse_minisat_status _ = Left "error in .cnf file: unexpected status line"

read_minisat_out :: String -> Either String (Maybe [Lit])
read_minisat_out s = parse_minisat_status $ lines s
--read_dimacs s = let x = parse_dimacs_status $ lines s in trace (show x) $ x

run_minisat :: FilePath -> FilePath -> IO (Bool, String, String)
run_minisat input_path output_path = do
  let args = ["-verb=2", input_path, output_path]
  (code, out, err) <-
    System.Process.readProcessWithExitCode minisat_path args ""
  case code of
    -- Based upon http://dev.man-online.org/man1/minisat
    System.Exit.ExitFailure 10 -> return (True, out, err)
    System.Exit.ExitFailure 20 -> return (False, out, err)
    _ -> Support.report_error ("minisat failed:\n" ++ out ++ err)

solve :: Bool -> CNF -> IO (Maybe [Lit])
solve keep_files cnf = do
  let dimacs = print_dimacs cnf
  (input_path, input_h) <- System.IO.openTempFile "." "board_in.cnf"
  System.IO.hPutStr input_h dimacs
  System.IO.hClose input_h
  (output_path, output_h) <- System.IO.openTempFile "." "board_out.cnf"
  System.IO.hClose output_h
  tic <- Data.Time.Clock.getCurrentTime
  (status, out, err) <- run_minisat input_path output_path
  tac <- Data.Time.Clock.getCurrentTime
  let dt = Data.Time.Clock.diffUTCTime tac tic
  putStrLn ("MiniSAT wall time: " ++ show dt)
  file_out <- readFile output_path
--  putStr file_out
  if keep_files
    then
      return ()
    else
      System.Directory.removeFile input_path
--      System.Directory.removeFile output_path  See https://ghc.haskell.org/trac/ghc/ticket/3231
  case read_minisat_out file_out of
    Left msg -> error msg
    Right sol -> return sol

