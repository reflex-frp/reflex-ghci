module MyLib (someFunc) where

import MyLib.One
import MyLib.Two
import MyLib.Three

someFunc :: IO ()
someFunc = putStrLn "someFunc"
