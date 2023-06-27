module MyLib where

import MyLib.One
import MyLib.Two
import MyLib.Three

someFunc :: IO ()
someFunc = putStrLn "someFunc"

err :: IO ()
err = do
  Just a <- return Nothing
  putStrLn a

done :: IO ()
done = putStrLn "done"
