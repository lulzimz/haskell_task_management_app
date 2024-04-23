module Task (
    Task(..)
) where

-- Define Task data type
data Task = Task {
    taskId :: Int,
    title :: String,
    description :: String,
    date :: String,
    priority :: Int,
    completed :: Bool
} deriving (Show)
