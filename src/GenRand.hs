import qualified System.Environment
import qualified System.Random
import qualified System.Exit
import qualified Data.Maybe

import qualified Board
import qualified Support

usage :: IO ()
usage = do
  progname <- System.Environment.getProgName
  putStrLn $ "Usage: runghc " ++ progname ++ " size numel [seed]"

parse_args :: System.Random.StdGen -> [String] -> Either String (Int, Int, System.Random.StdGen)
parse_args rdef args =
  let
    parse_args' :: [String] -> Either String (Int, Int, Maybe System.Random.StdGen)
    parse_args' [size, numel] =
      Right (read size, read numel, Nothing)
    parse_args' [size, numel, seed] =
      let
        r = System.Random.mkStdGen $ read seed
      in
        Right (read size, read numel, Just r)
    parse_args' _ = Left "unexpected number of arguments"
    res = parse_args' args
  in
    case res of
      Left msg -> Left msg
      Right (sz, n, r) ->
        if sz * sz < n
        then Left "number of elements is too big"
        else Right (sz, n, Data.Maybe.fromMaybe rdef r)

main :: IO ()
main = do
  rdef <- System.Random.getStdGen
  args <- System.Environment.getArgs 
  let parsed_args = parse_args rdef args
  case parsed_args of
    Left msg -> Support.report_error msg
    Right (size, numel, r) ->
      case Board.genrand size numel r of
        Left msg -> Support.report_error ("failed to generate board: " ++ msg)
        Right (b, _) -> putStr $ Board.pretty_print b

