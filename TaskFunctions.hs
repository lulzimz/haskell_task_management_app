module TaskFunctions (
    showMenu,
    performAction
) where

import Task
import TaskManipulation
import PrintColored

-- Display menu options in CLI|
showMenu :: IO ()
showMenu = do
    putStrLn "1. Create a task"
    putStrLn "2. Display Tasks list"
    putStrLn "3. Updated task status"
    putStrLn "4. Delete task"
    putStrLn "5. Display tasks by given status"
    putStrLn "6. Update task priority"
    putStrLn "7. Task statistics"
    putStrLn "0. Exit"
    putStrLn "Select one:"


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
    printColored colorBlue "Tasks list:"
    showTasks tasks
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction tasks userInput

performAction tasks "3" = do
    putStrLn "Enter task title to update:"
    taskTitle <- getLine
    putStrLn "Select new task status:"
    putStrLn "Options:"
    mapM_ putStrLn $ zipWith (\i status -> show i ++ ". " ++ status) [1..] validStatusOptions
    statusOption <- getLine
    let statusIndex = read statusOption :: Int
    if statusIndex >= 1 && statusIndex <= length validStatusOptions
        then do
            let newStatus = validStatusOptions !! (statusIndex - 1)
            let updatedTasks = map (\task -> if title task == taskTitle then updateTaskStatus task newStatus else task) tasks
            printColored colorGreen "Status changed!"
            putStrLn ""
            showMenu
            userInput <- getLine
            performAction updatedTasks userInput
        else do
            printColored colorRed "Invalid status option!"
            putStrLn ""
            performAction tasks "3"  -- Repeat the action if the status option is invalid

performAction tasks "4" = do
    putStrLn "Enter task title you want to delete:"
    taskTitle <- getLine
    let remainingTasks = deleteTask tasks taskTitle
    printColored colorGreen "Task deleted successfully!"
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction remainingTasks userInput

performAction tasks "5" = do
    putStrLn "Enter status to display tasks:"
    statusInput <- getLine
    let tasksByStatus = findTasksByStatus tasks statusInput
    printColored colorBlue $ "Tasks with status '" ++ statusInput ++ "':"
    showTasks tasksByStatus
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction tasks userInput

performAction tasks "6" = do
    putStrLn "Enter task title to change its priority:"
    taskTitle <- getLine
    putStrLn "Type new priority:"
    newPriority <- getLine
    let updatedTasks = map (\task -> if title task == taskTitle then updateTaskPriority task (read newPriority :: Int) else task) tasks
    printColored colorGreen "Priority updated!"
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction updatedTasks userInput

performAction tasks "7" = do
    putStrLn "Tasks statistics:"
    showTaskStatistics tasks
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction tasks userInput

performAction _ "0" = putStrLn "THANK YOU FOR USING OUR TASK MANAGEMENT APP!"

performAction tasks _ = do
    printColored colorRed "Selected option is invalid. Please choose one option from the list."
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction tasks userInput