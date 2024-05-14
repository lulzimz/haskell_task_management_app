module PrintColored
  ( printColored,
    colorRed,
    colorGreen,
    colorBlue,
  )
where

-- ANSI color codes
colorRed :: String
colorRed = "\x1b[31m"

colorGreen :: String
colorGreen = "\x1b[32m"

colorBlue :: String
colorBlue = "\x1b[34m"

colorReset :: String
colorReset = "\x1b[0m"

-- Function to print a string with color
printColored :: String -> String -> IO ()
printColored colorCode str = putStrLn $ colorCode ++ str ++ colorReset
