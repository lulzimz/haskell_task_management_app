
module UserInputActions (
    createTaskAction,
    displayTasksAction,
    updateStatusAction,
    deleteTaskAction,
    displayTasksByStatusAction,
    updateTaskPriorityAction,
    displayTaskStatisticsAction
) where

import Task
import TaskManipulation
import PrintColored

createTaskAction :: [Task] -> IO [Task]
createTaskAction tasks = do
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
    return (newTask:tasks)

displayTasksAction :: [Task] -> IO ()
displayTasksAction tasks = do
    printColored colorBlue "Tasks list:"
    showTasks tasks
    putStrLn ""

    -- Define a function to handle updating task status

updateStatusAction :: [Task] -> IO [Task]
updateStatusAction tasks = do
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
            return updatedTasks
        else do
            printColored colorRed "Invalid status option!"
            return tasks

deleteTaskAction :: [Task] -> IO [Task]
deleteTaskAction tasks = do
    putStrLn "Enter task title you want to delete:"
    taskTitle <- getLine
    let remainingTasks = deleteTask tasks taskTitle
    printColored colorGreen "Task deleted successfully!"
    return remainingTasks

displayTasksByStatusAction :: [Task] -> IO ()
displayTasksByStatusAction tasks = do
    putStrLn "Enter status to display tasks:"
    statusInput <- getLine
    let tasksByStatus = findTasksByStatus tasks statusInput
    printColored colorBlue $ "Tasks with status '" ++ statusInput ++ "':"
    showTasks tasksByStatus
    putStrLn ""

updateTaskPriorityAction :: [Task] -> IO [Task]
updateTaskPriorityAction tasks = do
    putStrLn "Enter task title to change its priority:"
    taskTitle <- getLine
    putStrLn "Type new priority:"
    newPriority <- getLine
    let updatedTasks = map (\task -> if title task == taskTitle then updateTaskPriority task (read newPriority :: Int) else task) tasks
    printColored colorGreen "Priority updated!"
    return updatedTasks

displayTaskStatisticsAction :: [Task] -> IO ()
displayTaskStatisticsAction tasks = do
    putStrLn "Tasks statistics:"
    showTaskStatistics tasks
    putStrLn ""
    