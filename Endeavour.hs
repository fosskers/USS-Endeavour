{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Servant.API
import Options.Generic
import Endeavour.Genetics

---

-- | Command-line arguments.
data Args = Args { config :: FilePath } deriving (Generic)

instance ParseRecord Args

main :: IO ()
main = do
  Args c <- getRecord "U.S.S. Endeavour - Computing Core"
  env <- genes c
  putStrLn c
  putStrLn "U.S.S. Endeavour - Computing Core activated."
