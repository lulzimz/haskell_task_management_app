import System.IO
import Data.List
import TaskFunctions

-- Main function to run the application
main :: IO ()
main = do
    putStrLn "WELCOME IN TAK MANAGEMENT APP!"
    showMenu
    userInput <- getLine
    performAction [] userInput
