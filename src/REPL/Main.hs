module REPL.Main
    ( repl
    ) where

import REPL.Solvers(silent,verbose,counting)
import Simple.UnifProblem(UnifProblem)
import Simple.Expression(Token(E))
import Simple.Substitution(identity,compose,restrict,onAny)

import System.Console.Haskeline

import REPL.Parser

data Verbosity = Verbose | Count | Silent deriving (Eq, Show)

toggle :: Verbosity -> Verbosity
toggle Silent = Count
toggle Count = Verbose
toggle Verbose = Silent

solveVS :: Verbosity -> (UnifProblem -> String)
solveVS Verbose = verbose
solveVS Silent = show . silent
solveVS Count  = counting

repl :: IO ()
repl = do
    putStrLn "Welcome!"
    putStrLn "Type an unifcation problem, :v to toggle verbosity or :q to quit."
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
                Just (Right (Apply subst expr)) -> do
                    outputStrLn (show $ subst `onAny` E expr)
                    loop v
                Just (Right (Compose substs)) -> do
                    outputStrLn (show $ restrict $ foldr compose identity substs)
                    loop v
