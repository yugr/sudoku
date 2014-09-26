import qualified System.Environment

import qualified Board
import qualified CNF
import qualified Support

main :: IO ()
main = do
  args <- System.Environment.getArgs 
  input <- Support.readFileOrStdin args
  let b = Board.pretty_read input
  let cnf = Board.to_cnf b
  putStr $ CNF.print_dimacs cnf

