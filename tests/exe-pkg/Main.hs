module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

err :: IO ()
err = error "This is an error"

done :: IO ()
done = putStrLn "Done"
