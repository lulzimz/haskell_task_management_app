module TaskManipulation (
    createTask,
    updateTaskStatus,
    deleteTask,
    showTasks
) where

import Task

--function to create a new task
createTask :: String -> String -> String -> Int -> Task
createTask title description date priority = Task {
    title = title,
    description = description,
    date = date,
    priority = priority,
    status = "To do"  -- Initial status is "To do"
}

-- Update function for a task
updateTaskStatus :: Task -> String -> Task
updateTaskStatus task newStatus = task { status = newStatus }

-- Delete function for a task
deleteTask :: [Task] -> String -> [Task]
deleteTask tasks taskTitle = filter (\task -> title task /= taskTitle) tasks

-- Display function for a task
showTasks :: [Task] -> IO ()
showTasks tasks = mapM_ print tasks