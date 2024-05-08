module TaskFunctions (
    showMenu,
    performAction
) where

import Task
import TaskManipulation

-- ANSI color codes
colorRed :: String
colorRed = "\x1b[31m"

colorGreen :: String
colorGreen = "\x1b[32m"

colorBlue :: String
colorBlue = "\x1b[34m"

colorReset :: String
colorReset = "\x1b[0m"

-- Function to print a string with color
printColored :: String -> String -> IO ()
printColored colorCode str = putStrLn $ colorCode ++ str ++ colorReset


------------------------------------ TASK MANIPULATION ------------------------------------ 


------------------------------------ TASK MANIPULATION ------------------------------------ 



------------------------------------ USER INTERACTIONS ------------------------------------ 
-- Display menu options in CLI|
showMenu :: IO ()
showMenu = do
    putStrLn "1. Create a task"
    putStrLn "2. Display Tasks list"
    putStrLn "3. Updated task status"
    putStrLn "4. Delete task"
    putStrLn "5. Exit"
    putStrLn "Select one:"

    

-- Perform action based on user input 
performAction :: [Task] -> String -> IO ()
performAction tasks "1" = do
    putStrLn "Enter title:"
    title <- getLine
    putStrLn "Enter description:"
    description <- getLine
    putStrLn "Enter date (example 2024-05-08):"
    date <- getLine
    putStrLn "Enter priority (integer):"
    priority <- getLine
    let newTask = createTask title description date (read priority :: Int)
    printColored colorGreen "Task created successfully!"
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction (newTask:tasks) userInput
performAction tasks "2" = do
    putStrLn "Tasks list:"
    showTasks tasks
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction tasks userInput
performAction tasks "3" = do
    putStrLn "Enter task title to update:"
    taskTitle <- getLine
    putStrLn "Enter new task status (example 'To do', 'In progress', 'Done'):"
    newStatus <- getLine
    let updatedTasks = map (\task -> if title task == taskTitle then updateTaskStatus task newStatus else task) tasks
    printColored colorGreen "Status changed!"
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction updatedTasks userInput
performAction tasks "4" = do
    putStrLn "Enter task title you want to delete:"
    taskTitle <- getLine
    let remainingTasks = deleteTask tasks taskTitle
    printColored colorGreen "Task deleted successfully!"
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction remainingTasks userInput
performAction _ "5" = putStrLn "THANK YOU FOR USING OUR TASK MANAGEMENT APP!"
performAction tasks _ = do
    printColored colorBlue "Selected option is invalid. Please choose one option from the list."
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction tasks userInput