module UserInputActions
  ( createTaskAction,
    displayTasksAction,
    updateStatusAction,
    deleteTaskAction,
    displayTasksByStatusAction,
    updateTaskPriorityAction,
    displayTaskStatisticsAction,
  )
where

import Distribution.Compat.Prelude (readMaybe)
import PrintColored
import Task
import TaskManipulation
import Validations

createTaskAction :: [Task] -> IO [Task]
createTaskAction tasks = do
  putStrLn "Enter title:"
  title <- getLine
  if isTitleUnique title tasks
    then do
      putStrLn "Enter description:"
      description <- getLine
      putStrLn "Enter date (YYYY-MM-DD):"
      date <- getDate
      putStrLn "Enter priority (integer):"
      priority <- getPriority
      let newTask = createTask title description date priority
      printColored colorGreen "Task created successfully!"
      return (newTask : tasks)
    else do
      printColored colorRed "Title already exists. Please enter a unique title."
      createTaskAction tasks

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
  if taskExists tasks taskTitle
    then do
      putStrLn "Select new task status:"
      mapM_ putStrLn $ zipWith (\i status -> show i ++ ". " ++ status) [1 ..] validStatusOptions
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
    else do
      printColored colorRed "Task with this title does not exists!"
      updateStatusAction tasks

deleteTaskAction :: [Task] -> IO [Task]
deleteTaskAction tasks = do
  putStrLn "Enter task title you want to delete:"
  taskTitle <- getLine
  if taskExists tasks taskTitle
    then do
      let remainingTasks = deleteTask tasks taskTitle
      printColored colorGreen "Task deleted successfully!"
      return remainingTasks
    else do
      printColored colorRed "Task with this title does not exists!"
      deleteTaskAction tasks

displayTasksByStatusAction :: [Task] -> IO ()
displayTasksByStatusAction tasks = do
  putStrLn "Enter status to display tasks:"
  mapM_ putStrLn $ zipWith (\i status -> show i ++ ". " ++ status) [1 ..] validStatusOptions
  selectedStatus <- getLine
  let statusIndex = read selectedStatus :: Int
  if statusIndex >= 1 && statusIndex <= length validStatusOptions
    then do
      let statusTitle = validStatusOptions !! (statusIndex - 1)
      let tasksByStatus = findTasksByStatus tasks statusTitle
      if not (null tasksByStatus)
        then do
          printColored colorGreen $ "Tasks with status '" ++ statusTitle ++ "':"
          showTasks tasksByStatus
        else do
          printColored colorBlue "No task found with this status!"
    else do
      printColored colorRed "Please enter only one of statuses above!"
      displayTasksByStatusAction tasks

updateTaskPriorityAction :: [Task] -> IO [Task]
updateTaskPriorityAction tasks = do
  putStrLn "Enter task title to change its priority:"
  taskTitle <- getLine
  if taskExists tasks taskTitle
    then do
      putStrLn "Type new priority:"
      priorityInput <- getPriority
      let updatedTasks = map (\task -> if title task == taskTitle then updateTaskPriority task priorityInput else task) tasks
      printColored colorGreen "Priority updated!"
      return updatedTasks
    else do
      printColored colorRed "Task with this title does not exists!"
      updateTaskPriorityAction tasks

displayTaskStatisticsAction :: [Task] -> IO ()
displayTaskStatisticsAction tasks = do
  putStrLn "Tasks statistics:"
  showTaskStatistics tasks
  putStrLn ""
