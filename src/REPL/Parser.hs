module REPL.Parser
    ( Command(..)
    , ReplCommand(..)
    , parseInput
    ) where

import REPL.Lexer

import Simple.Substitution
    ( Substitution
    )
import Simple.Expression
    ( Expr(Expr)
    , Bind((:=))
    , Var
    , var
    , meta
    )
import Simple.UnifProblem
    ( UnifProblem
    , UnifProblemEl((:=.:))
    )

import Text.Parsec
import Text.Parsec.String

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char(isUpper)


data Command = Command ReplCommand | Solve UnifProblem | Apply Substitution Expr deriving Show
data ReplCommand = Quit | SwitchVerbosity deriving Show


------------------------------------------------
-- Parser
------------------------------------------------

instruction :: Parser Command
instruction = replCommand <|> problem

parseInput :: String -> Command
parseInput input =
  case parse instruction "<stdin>" input of
    Left err -> error (show err)
    Right ast -> ast


------------------------------------------------
-- REPL Command
------------------------------------------------

quitCommand :: Parser ReplCommand
quitCommand = do
    char 'q'
    return Quit

switchVerbCommand :: Parser ReplCommand
switchVerbCommand = do
    char 'v'
    return SwitchVerbosity

replCommand :: Parser Command
replCommand = do
    char ':'
    c <- quitCommand <|> switchVerbCommand
    return $ Command c


------------------------------------------------
-- Unification Problem
------------------------------------------------
-- [X = a, B = C] =. [X = Y, A = x], [X = g] =. [b = g]

-- Easy creation of variables.
v :: Char -> Integer -> Var
v c = (if ($c) isUpper then meta else var) c

variable :: Parser Var
variable = do
    x <- letter
    n <- option 0 natural
    return (v x n)

bind :: Parser Bind
bind = do
    v1 <- lexeme variable
    lexeme (char '=')
    v2 <- lexeme variable
    return (v1 := v2)

expr :: Parser Expr
expr = do
    lexeme (char '[')
    bs <- commaSep bind
    lexeme (char ']')
    return (Expr bs)

problemEl :: Parser UnifProblemEl
problemEl = do
    e1 <- lexeme expr
    lexeme (string "=.")
    e2 <- lexeme expr
    return (e1 :=.: e2)

problem :: Parser Command
problem = do
    es <- commaSep problemEl
    return $ Solve (S.fromList es)


------------------------------------------------
-- Substitution Application
------------------------------------------------
-- {X -> a} [X = a]
-- {M -> [] | X -> a} [X = a]