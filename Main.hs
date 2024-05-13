import TaskFunctions

-- Main function to run the application
main :: IO ()
main = do
    putStrLn "WELCOME IN TASK MANAGEMENT APP!"
    showMenu
    userInput <- getLine
    performAction [] userInput
