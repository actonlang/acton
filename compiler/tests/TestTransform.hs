import qualified Acton.Parser as P
import System.Environment
import System.IO
import Pretty
import Acton.Printer
import Acton.Syntax
import Tests.ProtExtElim
import Tests.CPretty
import InterfaceFiles


main                  = do (_,m) <- P.parseModule (ModName [name "__builtin__"]) "__builtin__.act"
                           let m1  = transform m
                           putStrLn (render(cprint m1))



