module REPL.Main
    ( repl
    ) where

import Simple.UnifProblem(UnifProblem)
import Simple.Algorithm(solve,solveVerbose)

import System.Console.Haskeline

import REPL.Parser

data Verbosity = Verbose | Silent deriving (Eq, Show)

toggle :: Verbosity -> Verbosity
toggle Verbose = Silent
toggle Silent = Verbose

solveVS :: Verbosity -> (UnifProblem -> String)
solveVS Verbose = solveVerbose
solveVS Silent = show . solve

repl :: IO ()
repl = do
    putStrLn "Welcome!"
    putStrLn "Type an unifcation problem, :v to toggle verbosity or :q to quit."
    putStrLn "Example:  [X = a, B = C] =. [X = Y, A = x], [X = g] =. [b = g]"
    runInputT defaultSettings (loop Silent)
    where
        loop :: Verbosity -> InputT IO ()
        loop v = do
            minput <- getInputLine "> "
            case parseInput <$> minput of
                Nothing -> return ()
                Just (Left err) -> do
                    outputStrLn err
                    loop v
                Just (Right (Command Quit)) -> return ()
                Just (Right (Command SwitchVerbosity)) -> do
                    outputStrLn $ "Switched verbosity to: " ++ show (toggle v)
                    loop (toggle v)
                Just (Right (Solve problem)) -> do
                    outputStrLn (solveVS v problem)
                    loop v
