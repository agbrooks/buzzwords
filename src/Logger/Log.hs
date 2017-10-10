module Logger.Log (
  logArbitrary,
  logInfo,
  logWarn,
  logErr
  ) where

import System.Console.ANSI
import System.IO ( Handle
                 , hPutStr
                 , stdout
                 , stderr )

-- | Make a log message resembling "[tag]: message." 
logArbitrary :: Color  -> -- ^Color that the tag should appear in
                Handle -> -- ^Stdin/Stdout
                String -> -- ^Tag text
                String -> -- ^Message to log
                IO ()
                
logArbitrary c h tag msg = do
  setSGR [Reset]
  hPutStr h "["
  setSGR [SetColor Foreground Vivid c]
  hPutStr tag
  setSGR [Reset]
  hPutStr h "]: "
  hPutStr h (msg ++ "\n")

-- | Log an informative message to console.
logInfo :: String -> IO ()
logInfo = logArbitrary Cyan stdout "INFO"

-- | Log a warning to console.
logWarn :: String -> IO ()
logWarn = logArbitrary Yellow stderr "WARN"

-- | Log an error to console.
logErr :: String -> IO ()
logErr = logArbitrary Red stderr "ERR!"
