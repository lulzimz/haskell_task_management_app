module TaskManipulation
  ( validStatusOptions,
    createTask,
    updateTaskStatus,
    deleteTask,
    showTasks,
    findTasksByStatus,
    updateTaskPriority,
    showTaskStatistics,
    taskExists,
  )
where

import PrintColored
import Task

-- Define a list of valid status options
validStatusOptions :: [String]
validStatusOptions = ["To do", "In progress", "Done"]

--------------------------------- Task Manipulations--------------------------
-- function to create a new task
createTask :: String -> String -> String -> Int -> Task
createTask title description date priority =
  Task
    { title = title,
      description = description,
      date = date,
      priority = priority,
      status = "To do" -- Initial status is "To do"
    }

-- Update task status
updateTaskStatus :: [Task] -> String -> String -> IO [Task]
updateTaskStatus tasks newStatus taskTitle = do
  let updatedTasks = map (\task -> if title task == taskTitle then task {status = newStatus} else task) tasks
  printColored colorGreen "Status changed!"
  return updatedTasks

-- Delete function for a task
deleteTask :: [Task] -> String -> [Task]
deleteTask tasks taskTitle = filter (\task -> title task /= taskTitle) tasks

-- Display function for a task
showTasks :: [Task] -> IO ()
showTasks = mapM_ print

-- Find task by status
findTasksByStatus :: [Task] -> String -> [Task]
findTasksByStatus tasks taskStatus = filter (\task -> taskStatus == status task) tasks

-- check if task exists
taskExists :: [Task] -> String -> Bool
taskExists tasks taskTitle = any (\task -> title task == taskTitle) tasks

-- change task priority
updateTaskPriority :: Task -> Int -> Task
updateTaskPriority task newPriority = task {priority = newPriority}

-- Show statistics
showTaskStatistics :: [Task] -> IO ()
showTaskStatistics tasks = do
  let totalTasks = length tasks
  let completedTasks = length (filter (\task -> status task == "Done") tasks)
  let pendingTasks = length (filter (\task -> status task /= "Done") tasks)
  printColored colorBlue $ "Total tasks: " ++ show totalTasks
  printColored colorBlue $ "Done Tasks: " ++ show completedTasks
  printColored colorBlue $ "To do Tasks: " ++ show pendingTasks
