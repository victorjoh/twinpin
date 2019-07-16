module Main where
import           Control.Monad.Loops

main :: IO ()
main = iterateUntilM_ shouldExit displayLoop initWorld

iterateUntilM_ :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateUntilM_ p f v = do
    iterateUntilM p f v
    return ()

displayLoop :: Int -> IO Int
displayLoop w = do
    displayWorld w
    return (gameLoop w)

shouldExit :: Int -> Bool
shouldExit i = 10 < i

initWorld :: Int
initWorld = 0

gameLoop :: Int -> Int
gameLoop = (+) 1

displayWorld :: Int -> IO ()
displayWorld = print
