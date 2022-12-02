module Lib (someFunc) where

import           HDL.Interpreter (testChip)

someFunc :: IO ()
someFunc = testChip
