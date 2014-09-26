import qualified System.Environment

import qualified Support
import qualified Board

main :: IO ()
main = do
  args <- System.Environment.getArgs 
  input <- Support.readFileOrStdin args
  let b = Board.pretty_read input
  sol <- Board.solve b
  putStr $ case sol of
    Nothing -> "solution does not exist"; Just b -> Board.pretty_print b

