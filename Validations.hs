module Validations
  ( getPriority,
    getDate,
    isTitleUnique,
    getStatusOptions,
    getTitle,
  )
where

import Data.List (find)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import PrintColored
import Task
import TaskManipulation
import Text.Read (readMaybe)

------------------------- Title Validation -------------------------
isTitleUnique :: String -> [Task] -> Bool
isTitleUnique taskTitle tasks = null $ find (\task -> title task == taskTitle) tasks

-- Validate priority input for integer
getTitle :: [Task] -> IO String
getTitle tasks = do
  putStrLn "Enter task title:"
  taskTitle <- getLine
  if taskExists tasks taskTitle
    then do
      return taskTitle
    else do
      printColored colorRed "Task with this title does not exists!"
      getTitle tasks

------------------------- Priority Validation -------------------------
-- Validate priority input for integer
getPriority :: IO Int
getPriority = do
  priorityStr <- getLine
  case readMaybe priorityStr :: Maybe Int of
    Just priority -> return priority
    Nothing -> do
      printColored colorRed "Invalid priority! Please enter an integer:"
      getPriority

------------------------- Date Validation -------------------------
getDate :: IO String
getDate = do
  date <- getLine
  if isValidDateFormat date
    then return date
    else do
      printColored colorRed "Invalid date format. Please use YYYY-MM-DD format."
      getDate

isValidDateFormat :: String -> Bool
isValidDateFormat dateStr =
  length dateStr == 10
    && case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr :: Maybe UTCTime of
      Just _ -> True
      Nothing -> False

------------------------- Status Validation -------------------------
getStatusOptions :: [Task] -> IO String
getStatusOptions tasks = do
  putStrLn "Select task status:"
  mapM_ putStrLn $ zipWith (\i status -> show i ++ ". " ++ status) [1 ..] validStatusOptions
  statusOption <- getLine
  let statusIndex = read statusOption :: Int
  if statusIndex >= 1 && statusIndex <= length validStatusOptions
    then do
      let selectedStatus = validStatusOptions !! (statusIndex - 1)
      return selectedStatus
    else do
      printColored colorRed "Invalid status option!"
      getStatusOptions tasks
