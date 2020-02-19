import qualified Acton.Parser as P
import System.Environment
import System.IO
import Pretty
import Acton.Printer
import Acton.Syntax
import Acton.ProtExtElim



main = do file:_ <- getArgs
          (src,tree) <- P.parseModule (modName ["__"]) file
          let m = transform tree
          putStrLn (render (pretty m))
 