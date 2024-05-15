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

-- Function to handle user inputs and create a task
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

-- Function to handle updating task status
updateStatusAction :: [Task] -> IO [Task]
updateStatusAction tasks = do
  taskTitle <- getTitle tasks
  selectedStatus <- getStatusOptions tasks
  updateTaskStatus tasks selectedStatus taskTitle

deleteTaskAction :: [Task] -> IO [Task]
deleteTaskAction tasks = do
  taskTitle <- getTitle tasks
  let remainingTasks = deleteTask tasks taskTitle
  printColored colorGreen "Task deleted successfully!"
  return remainingTasks

displayTasksByStatusAction :: [Task] -> IO ()
displayTasksByStatusAction tasks = do
  selectedStatus <- getStatusOptions tasks
  let tasksByStatus = findTasksByStatus tasks selectedStatus
  if not (null tasksByStatus)
    then do
      printColored colorGreen $ "Tasks with status '" ++ selectedStatus ++ "':"
      showTasks tasksByStatus
    else do
      printColored colorBlue "No task found with this status!"

updateTaskPriorityAction :: [Task] -> IO [Task]
updateTaskPriorityAction tasks = do
  taskTitle <- getTitle tasks
  putStrLn "Type new priority:"
  priorityInput <- getPriority
  let updatedTasks = map (\task -> if title task == taskTitle then updateTaskPriority task priorityInput else task) tasks
  printColored colorGreen "Priority updated!"
  return updatedTasks

displayTaskStatisticsAction :: [Task] -> IO ()
displayTaskStatisticsAction tasks = do
  putStrLn "Tasks statistics:"
  showTaskStatistics tasks
  putStrLn ""
