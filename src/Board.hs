module Board (Board, genrand, size, pretty_print, pretty_read, solve, to_cnf) where

import qualified System.Random

import qualified Data.List
import qualified Data.Maybe
import qualified Data.Array
import Data.Array ((!),(//))

import Debug.Trace (trace)
import qualified Control.DeepSeq
import Control.DeepSeq (($!!))

import qualified Support
import qualified CNF

type BoardElt = Maybe Int
type Board = (Int, Data.Array.Array Int BoardElt)

indices (size, _) = [0 .. size - 1]
index_pairs b = [(i, j) | i <- indices b, j <- indices b]
encode_idx (size, _) (i, j) = i * size + j
decode_idx (size, _) i =
  let row = i `mod` size in (row, i - row * size)

block_size size = Support.isqrt size

ij2block (size, _) i j =
  let
    bs = block_size size
  in
    (div i bs, div j bs)

block_ijs b@(size, m) i j =
  let
    bs = block_size size
  in
    [(i', j') | di <- [0 .. bs - 1],
                dj <- [0 .. bs - 1],
                let i' = i * bs + di,
                let j' = j * bs + dj]

-- Get row of board
row :: Board -> Int -> [Maybe Int]
row b@(size, m) i =
  [x | j <- indices b,
       let idx = encode_idx b (i, j),
       let x = m ! idx]

-- Same but with non-initialized elements removed
row_short :: Board -> Int -> [Int]
row_short b i = Data.Maybe.catMaybes $ row b i

-- Same for columns
col :: Board -> Int -> [Maybe Int]
col b@(size, m) j =
  [x | i <- indices b,
       let idx = encode_idx b (i, j),
       let x = m ! idx]

-- Same for columns
col_short :: Board -> Int -> [Int]
col_short b i = Data.Maybe.catMaybes $ col b i

-- Get neighbors of element
block :: Board -> (Int, Int) -> [Maybe Int]
block b@(size, m) (i, j) =
  [x | ij <- block_ijs b i j,
       let idx = encode_idx b ij,
       let x = m ! idx]

-- Same but with non-initialized elements removed
block_short :: Board -> (Int, Int) -> [Int]
block_short b ij = Data.Maybe.catMaybes $ block b ij

-- Can we place k at (i, j) in board?
is_good_position :: (Int, Int) -> Int -> Board -> Bool
is_good_position (i, j) k b@(size, m) =
  let
    no_conflicts = notElem k
    old = m ! encode_idx b (i, j)
    bij = ij2block b i j
  in
    Data.Maybe.isNothing old && no_conflicts (row_short b i) && no_conflicts (col_short b j) && no_conflicts (block_short b bij)

-- Helper for genrand: put n random elements on board
put_on_board :: Board -> Int -> Int -> System.Random.StdGen -> Either String (Board, System.Random.StdGen)
put_on_board _ _ 0 _ = Left "size is probably too big for generator"
put_on_board b 0 _ r = Right (b, r)
put_on_board b@(size, m) n limit r =
  let
    bnds = (0, size - 1)
    (i, r2) = System.Random.randomR bnds r
    (j, r3) = System.Random.randomR bnds r2
    (k, r4) = System.Random.randomR (1, size) r3
    idx = encode_idx b (i, j)
    m' = m // [(idx, Just k)]
    limit' = limit - 1
  in
    if is_good_position (i, j) k b
      then put_on_board (size, m') (n - 1) limit' r4
      else put_on_board b n limit' r4

-- Generate board with n random elements
genrand :: Int -> Int -> System.Random.StdGen -> Either String (Board, System.Random.StdGen)
genrand size n r
    | bs * bs /= size = Left "board size must be a square"
    | otherwise = put_on_board (size, m) n limit r
  where
    m = Support.filled (0, size * size - 1) Nothing
    limit = 10000
    bs = Support.isqrt size

-- Returns board size
size :: Board -> Int
size (s, _) = s

pretty_print :: Board -> String
pretty_print b@(size, m) =
  let
    ndigits = length $ show size
    pad = Support.pad ndigits ' '
    print_elt x = pad $
      case x of
        Nothing -> "."
        Just y -> Prelude.show y
    print_row i = unwords (map print_elt $ row b i) ++ "\n"
  in
    concatMap print_row [0 .. size - 1]

pretty_read :: String -> Board
pretty_read s =
  let
    rows = Support.deintersperse_multi '\n' s
    cols = map (Support.deintersperse_multi ' ') rows
    num_rows = length rows
    num_cols = map length cols
    size = if all (== num_rows) num_cols
      then num_rows
      else error "non-matching number of rows/columns"
    read_elt x = if x == "." then Nothing else Just $ read x
    mapmap f = map (map f)
    nums = mapmap read_elt cols
    -- FIXME: slow
    inits =
      [(idx, x) | i <- [0 .. size - 1],
                  j <- [0 .. size - 1],
                  let idx = i * size + j,
                  let x = nums !! i !! j]
  in
    (size, Data.Array.array (0, size * size - 1) inits)
{-
   This is a first try on generating CNF from Sudoku.

   Let N denote the size of the board.
   Each position (total N*N positions) gets N boolean variables,
   Si,j(k) one per each possible number in this position.
   The number of variables is thus N^3.

   Then we can formalize Sudoku as
     (1) Si,j(k) -> not Si,j(k')  for all k' /= k
     (2) Si,j(k) -> not Si',j(k)  for all i' /= i
     (3) Si,j(k) -> not Si,j'(k)  for all j' /= i
     (4) Si,j(k) -> not Si',j'(k) for all i', j' in surrounding block
     (5) Si,j(k) for some k
   We can rewrite these for CNF form:
     (1) not Si,j(k) or not Si,j(k')
     (2) not Si,j(k) or not Si',j(k)
     (3) not Si,j(k) or not Si,j'(k)
     (4) not Si,j(k) or not Si',j'(k)
     (5) Si,j(1) or Si,j(2) or ...
   Each rule scheme gives N^4 equations. Problem sizes for small boards:
    * 26244 for 9x9 boards
    * 262144 for 16x16 boards
    * 1562500 for 25x25 boards
    * 6718464 for 36*36 boards
-}

nvars (size, _) = size * size * size
encode (size, _) i j k = i * size * size + j * size + k
decode (size, _) idx = let idx1 = idx - 1 in (idx1 `mod` size * size, idx1 `mod` size, rem idx1 size + 1)

to_cnf :: Board -> CNF.CNF
to_cnf b@(size, m) = rules
  where
    rule1 i j k k' = [CNF.notlit (encode b i j k), CNF.notlit (encode b i j k')]
    rule2 i j k i' = [CNF.notlit (encode b i j k), CNF.notlit (encode b i' j k)]
    rule3 i j k j' = [CNF.notlit (encode b i j k), CNF.notlit (encode b i j' k)]
    rule4 i j k i' j' = [CNF.notlit (encode b i j k), CNF.notlit (encode b i' j' k)]
    rule5 i j = map (\k -> CNF.lit $ encode b i j k) [1 .. size]
    fact (i, j) =
      case m ! encode_idx b (i, j) of
        Nothing -> Nothing
        Just k -> Just [CNF.lit (encode b i j k)]

    ijks = [(i, j, k) | i <- [0 .. size - 1], j <- [0 .. size - 1], k <- [1 .. size]]

    rules = rules1 ++ rules2 ++ rules3 ++ rules4 ++ rules5 ++ facts

    rules1 =
      [rule1 i j k k' | (i, j, k) <- ijks, k' <- [1 .. k - 1]]

    rules2 =
      [rule2 i j k i' | (i, j, k) <- ijks, i' <- [0 .. i - 1]]

    rules3 =
      [rule3 i j k j' | (i, j, k) <- ijks, j' <- [0 .. j - 1]]

    rules4 =
      [rule4 i j k i' j' | (i, j, k) <- ijks,
                           let idx = encode_idx b (i, j),
                           let (bi, bj) = ij2block b i j,
                           (i', j') <- block_ijs b bi bj,
                           let idx' = encode_idx b (i', j'),
                           idx' > idx]

    rules5 = [rule5 i j | i <- [0 .. size - 1], j <- [0 .. size - 1]]

    facts = Data.Maybe.mapMaybe fact $ index_pairs b

-- Helper for solve
get_single_yes :: Board -> [CNF.Lit] -> Int
get_single_yes b cnf
--    | trace ("get_assign:\n  cnf = " ++ (show cnf) ++ "\n  pos = " ++ (show pos) ++ "\n  negs = " ++ (show negs) ++ "\n") False = undefined
    | length yes /= 1 = error "invalid input from solver: number of positive variables for cell /= 1"
    | otherwise = k
  where
    (_, yes) = Data.List.partition CNF.is_negation cnf
    CNF.Yes var = head yes
    (_, _, k) = decode b var

conflict :: Board -> Board -> Bool
conflict (size1, m1) (size2, m2)
    | size1 /= size2 = error "unexpected arguments"
    | otherwise = any (\i -> conflict' (m1 ! i) (m2 ! i)) $ Data.Array.indices m1
  where
    conflict' Nothing _ = False
    conflict' _ Nothing = False
    conflict' (Just x) (Just y) = x /= y

-- Helper for solve
from_cnf b@(size, m) cnf
    | length cnf /= size * size * size = error "invalid input from solver: number of variables not matching board"
    | conflict b b' = error "invalid input from solver: original board contents not preserved"
    | otherwise = b'
  where
    chunks = Support.chunks cnf size  -- Each cell has size variables assigned
    vals = map (get_single_yes b) chunks
    m' = Data.Array.array (0, size * size - 1) $ zip [0 .. size * size - 1] (map Just vals)
    b' = (size, m')

-- Solve Sudoku!
solve :: Board -> IO (Maybe Board)
solve b@(size, m) = do
  sol <- CNF.solve False $!! to_cnf b
  case sol of
    Nothing -> return Nothing
    Just cnf -> return $ Just $ from_cnf b cnf

