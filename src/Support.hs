module Support where

import qualified System.Environment
import qualified System.Exit
import qualified Data.Array
import Debug.Trace (trace)

-- Create Data.Array.Array with all elements initialized to some value
filled :: (Data.Array.Ix a) => (a, a) -> b -> Data.Array.Array a b
filled bnds x = Data.Array.array bnds [(i, x) | i <- Data.Array.range bnds]

-- From http://www.haskell.org/ghc/docs/6.12.1/html/users_guide/assertions.html
arsert :: Bool -> a -> a
arsert False _ = error "assertion failed"
arsert True x = x

-- Inverse of Data.List.intersperse: split list at value
deintersperse :: (Eq a) => a -> [a] -> [[a]]
deintersperse x xs =
  let
    deintersperse' [] acc = [acc]
    deintersperse' (y:ys) acc =
      if x == y
        then acc : deintersperse' ys []
        -- FIXME: slow
        else deintersperse' ys (acc ++ [y])
  in
    deintersperse' xs []

-- Same as deintersperse but treat multiple separators as single one
deintersperse_multi :: (Eq a) => a -> [a] -> [[a]]
deintersperse_multi x xs =
  filter (not . null) $ deintersperse x xs

-- Helper for main functions: if cmdline args contain filename - read it, otherwise read stdin
readFileOrStdin :: [String] -> IO String
readFileOrStdin args =
  case args of
    arg:_ -> readFile arg
    [] -> getContents

-- Print user-friendly error and exit
report_error :: String -> IO a
report_error msg = do
  progname <- System.Environment.getProgName
  putStrLn $ progname ++ ": " ++ msg
  _ <- System.Exit.exitFailure
  return undefined

-- Drop last element of list
drop_last :: [a] -> [a]
drop_last [_] = []
drop_last (x:xs) = x:drop_last xs

trace_it :: (Show a) => a -> a
trace_it x = trace (show x) x

-- Split list to equally-sized chunks
chunks :: [a] -> Int -> [[a]]
chunks [] _ = []
chunks xs n = take n xs : chunks (drop n xs) n

-- Left-pad list to a given size
pad :: Int -> a -> [a] -> [a]
pad n x xs =
    replicate nx x ++ xs
  where
    nx = max 0 (n - length xs)

-- Integer sqrt
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

