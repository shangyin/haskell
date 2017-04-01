import System.Environment
import Control.Monad

main = do
    argLists <- getArgs
    mapM_ putStrLn argLists

