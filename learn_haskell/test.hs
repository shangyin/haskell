import System.Environment
import Control.Exception
import System.Random
import System.Directory
import System.IO
import Data.List
import Control.Monad(when)

dispath :: [String] -> [String] -> IO()
dispath ["add"] = add
dispath ["view"] = view
dispath ["remove"] = remove
dispath ["bump"] = bump
dispath [] = nothing


amain = do
    argList <- getArgs
    let command = take 1 argList
        arg = drop 1 argList 
    dispath command arg 

add :: [String] -> IO()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")


view :: [String]-> IO()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks


remove :: [String] -> IO()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    mapM_ putStrLn numberedTasks
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do 
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)


bump :: [String]-> IO()
bump [fileName, number] = do
    contents <- readFile fileName
    let toTop = lines contents !! read number
        newTodoTasks = toTop : delete toTop ( lines contents)
    putStr . unlines $ zipWith (\n line -> show n ++ " - " ++ line) [0..] newTodoTasks 
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle $ unlines newTodoTasks
            hClose tempHandle
            removeFile fileName 
            renameFile tempName fileName)
        

nothing :: [String] -> IO()
nothing _ = do
    putStrLn "input a supported command"    


main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO()
askForNumber gen = do
    let (randNum, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStr "1 to 10"
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString :: Int
        if randNum == number
            then putStrLn "yes"
            else putStrLn $ "no, its " ++ show randNum
        askForNumber newGen
