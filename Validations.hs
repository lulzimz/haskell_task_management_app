module Validations
  ( getPriority,
    getDate,
    isTitleUnique,
  )
where

import Data.List (find)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import PrintColored
import Task
import Text.Read (readMaybe)

------------------------- Title Validation -------------------------
isTitleUnique :: String -> [Task] -> Bool
isTitleUnique taskTitle tasks = null $ find (\task -> title task == taskTitle) tasks

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