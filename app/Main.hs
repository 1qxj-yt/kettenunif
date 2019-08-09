module Main where

import Simple.Algorithm(solve, solveVerbose)
import Simple.UnifProblem
import Simple.Expression

import System.Console.Haskeline

import System.IO
import Data.Char(isSpace,isUpper)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Set as S


data Verbosity = Verbose | Silent deriving (Eq, Show)

toggle :: Verbosity -> Verbosity
toggle Verbose = Silent
toggle Silent = Verbose

solveVS :: Verbosity -> (UnifProblem -> String)
solveVS Verbose = solveVerbose
solveVS Silent = show . solve


main :: IO ()
main = do
    putStrLn "Welcome!"
    putStrLn "Type an unifcation problem, :v to toggle verbosity or :q to quit."
    putStrLn "Example:  [X = a; B = C] =. [X = Y; A = x], [X = g] =. [b = g]"
    runInputT defaultSettings (loop Silent)
    where
        loop :: Verbosity -> InputT IO ()
        loop v = do
            minput <- getInputLine "> "
            case minput of
                Nothing -> return ()
                Just ":q" -> return ()
                Just ":v" -> do
                    outputStrLn $ "Switched verbosity to: " ++ show (toggle v)
                    loop (toggle v)
                Just input -> do
                    case parseProb (BC.pack input) of
                        Right prob -> outputStrLn (solveVS v prob)
                        Left str -> outputStrLn str
                    loop v


-- Easy creation of variables.
v :: Char -> Var
v c = (if ($c) isUpper then meta else var) c 0


parseProb :: BC.ByteString -> Either String UnifProblem
parseProb str = do
    let unspaced = unSpace str      :: BC.ByteString
        elSet    = probEl unspaced  :: S.Set BC.ByteString
    exprTupSet  <- S.fromList <$> mapM expr (S.toList elSet) :: Either String (S.Set (BC.ByteString, BC.ByteString))
    S.fromList <$> mapM (\(l,r) -> do
                        exprL <- parseExpr l
                        exprR <- parseExpr r
                        return (exprL :=.: exprR )
                    ) (S.toList exprTupSet)

parseExpr :: BC.ByteString -> Either String Expr
parseExpr str = do
    unbracked <- unbracket str          :: Either String BC.ByteString
    binds <- return (bind unbracked)    :: Either String [BC.ByteString]
    vared <- mapM vari binds            :: Either String [(Char,Char)]
    return $ map (\(l,r)-> v l := v r) vared


unSpace :: BC.ByteString -> BC.ByteString
unSpace = BC.filter (not.isSpace)

probEl :: BC.ByteString -> S.Set BC.ByteString
probEl = S.fromList . BC.split ','

expr :: BC.ByteString -> Either String (BC.ByteString, BC.ByteString)
expr str = case BC.split '.' str of
    [l,r]   -> if BC.last l == '='
                    then Right (BC.init l, r)
                    else Left $ "Could not detect problem element: " ++ BC.unpack str
    _       -> Left $ "Could not detect problem element: " ++ BC.unpack str

unbracket :: BC.ByteString -> Either String BC.ByteString
unbracket str = if BC.head str == '[' && BC.last str == ']'
    then Right $ BC.init (BC.tail str)
    else Left $ "Failed to unbracket expression: " ++ BC.unpack str

bind :: BC.ByteString -> [BC.ByteString]
bind = BC.split ';'

vari :: BC.ByteString -> Either String (Char, Char)
vari str = case BC.split '=' str of
    [l,r]   -> if BC.length l == 1 && BC.length r == 1
                then Right (BC.head l, BC.head r) else Left "Variable length should be one."
    ls      -> Left $ "Too many equalities in binding: " ++ show (length ls) ++ " instead of two."
