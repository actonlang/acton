import qualified Acton.Parser as P
import System.Environment
import System.IO
import Pretty
import Acton.Printer
import Acton.Syntax
import Tests.ProtExtElim2
import Tests.CPretty
import InterfaceFiles


main                  = do tenv <- InterfaceFiles.readFile "__builtin__.ty"
                           let (tenv',ps) = transform tenv
                           putStrLn (render(cprettyEnv ps tenv'))



