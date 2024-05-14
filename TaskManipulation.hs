module TaskManipulation
  ( validStatusOptions,
    createTask,
    updateTaskStatus,
    deleteTask,
    showTasks,
    findTasksByStatus,
    updateTaskPriority,
    showTaskStatistics,
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

-- Update function for a task
updateTaskStatus :: Task -> String -> Task
updateTaskStatus task newStatus = task {status = newStatus}

-- Delete function for a task
deleteTask :: [Task] -> String -> [Task]
deleteTask tasks taskTitle = filter (\task -> title task /= taskTitle) tasks

-- Display function for a task
showTasks :: [Task] -> IO ()
showTasks tasks = mapM_ print tasks

-- Find task by status
findTasksByStatus :: [Task] -> String -> [Task]
findTasksByStatus tasks taskStatus = filter (\task -> taskStatus == status task) tasks

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
