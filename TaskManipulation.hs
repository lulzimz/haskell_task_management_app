module TaskManipulation (
    addTask,
) where

import Task
import TaskFunctions  

--function to add a task to a list of tasks
addTask :: [Task] -> Task -> [Task]
addTask tasks task = task { taskId = nextTaskId tasks } : tasks
