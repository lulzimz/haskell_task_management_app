module Task (
    Task(..)
) where

-- Define Task data type
data Task = Task {
    title :: String,
    description :: String,
    date :: String,
    priority :: Int,
    status :: String
} deriving (Show)
