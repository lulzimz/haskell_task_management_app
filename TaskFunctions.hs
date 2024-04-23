module TaskFunctions (
    createTask,
    nextTaskId
) where

import Task

--function to create a new task
createTask :: String -> String -> String -> Int -> Task
createTask title description date priority = Task {
    taskId = 0,  
    title = title,
    description = description,
    date = date,
    priority = priority,
    completed = False
}

--function to generate unique task IDs to get max taskId and add 1 
nextTaskId :: [Task] -> Int
nextTaskId tasks = if null tasks then 1 else maximum (map taskId tasks) + 1

