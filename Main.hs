import Task
import TaskFunctions
import TaskManipulation

-- Main function to run the application
main :: IO ()
main = do
    --create a task
    let task1 = createTask "Test Task 1" "Description for Task 1" "2024-04-23" 1

    --add the task to a list of tasks
    let initialTasks = addTask [] task1

    --print the list of tasks
    putStrLn "Initial Tasks:"
    print initialTasks