module TaskFunctions (
    showMenu,
    performAction
) where

import Task
import TaskManipulation
import PrintColored
import UserInputActions

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
    newTask <- createTaskAction tasks
    showMenu >>= \_ -> getLine >>= \userInput -> performAction newTask userInput --operator >>= (bind) is used to chain actions together

performAction tasks "2" = do
    displayTasksAction tasks
    showMenu >>= \_ -> getLine >>= \userInput -> performAction tasks userInput

performAction tasks "3" = do
    updatedTasks <- updateStatusAction tasks
    putStrLn ""
    showMenu >>= \_ -> getLine >>= \userInput -> performAction updatedTasks userInput

performAction tasks "4" = do
    updatedTasks <- deleteTaskAction tasks
    showMenu >>= \_ -> getLine >>= \userInput -> performAction updatedTasks userInput

performAction tasks "5" = do
    displayTasksByStatusAction tasks
    showMenu >>= \_ -> getLine >>= \userInput -> performAction tasks userInput

performAction tasks "6" = do
    updatedTasks <- updateTaskPriorityAction tasks
    showMenu >>= \_ -> getLine >>= \userInput -> performAction updatedTasks userInput

performAction tasks "7" = do
    displayTaskStatisticsAction tasks
    showMenu >>= \_ -> getLine >>= \userInput -> performAction tasks userInput
    
performAction _ "0" = putStrLn "THANK YOU FOR USING OUR TASK MANAGEMENT APP!"

performAction tasks _ = do
    printColored colorRed "Selected option is invalid. Please choose one option from the list."
    putStrLn ""
    showMenu
    userInput <- getLine
    performAction tasks userInput