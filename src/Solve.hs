import qualified System.Environment

import qualified Control.DeepSeq
import Control.DeepSeq (($!!))

import qualified Support
import qualified Board

main :: IO ()
main = do
  args <- System.Environment.getArgs 
  input <- Support.readFileOrStdin args
  let b = Control.DeepSeq.force $ Board.pretty_read input
  sol <- Board.solve $ b
  putStr $ case sol of
    Nothing -> "solution does not exist"; Just b -> Board.pretty_print b

