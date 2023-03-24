import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let source = args !! 0
      target = args !! 1
  input <- TIO.readFile source
  TIO.writeFile target input
