 {-# LANGUAGE TemplateHaskell #-}
module Main where
--Gets the structure of the Database before compiling main program
import HLINQ



$(createDB "../test.db" "test")

