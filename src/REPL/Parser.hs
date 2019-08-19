module REPL.Parser
    ( Command(..)
    , ReplCommand(..)
    , parseInput
    ) where

import REPL.Lexer

import Simple.Substitution
    ( Substitution
    , (→)
    , (→→)
    , identity
    , build
    , compose
    )
import Simple.Expression
    ( Expr(Expr,SingleSVarExpr)
    , Bind((:=))
    , Var
    , SetVar(SetVar)
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


data Command =    Command ReplCommand
                | Solve UnifProblem
                | Apply Substitution Expr
                | Compose [Substitution]
                    deriving Show
data ReplCommand = Quit | SwitchVerbosity deriving Show


------------------------------------------------
-- Parser
------------------------------------------------

instruction :: Parser Command
instruction = do
    whiteSpace
    -- 'problem' choice should be placed at the end,
    -- otherwise, the problem-parser comsumes input
    cmd <- substAppl <|> replCommand <|> problem
    eof
    return cmd

parseInput :: String -> Either String Command
parseInput input =
  case parse instruction "<stdin>" input of
    Left err -> Left (show err)
    Right ast -> Right ast


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

setVar :: Parser SetVar
setVar = do
    char 'M'
    n <- option 0 natural
    return (SetVar n)

singleSetExpr :: Parser Expr
singleSetExpr = do
    sv <- setVar
    char ':'
    Expr bs <- expr
    return $ SingleSVarExpr sv bs

problemEl :: Parser UnifProblemEl
problemEl = do
    e1 <- lexeme (expr <|> singleSetExpr)
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

assocVar :: Parser Substitution
assocVar = do
    v1 <- lexeme variable
    lexeme (string "->")
    v2 <- lexeme variable
    return (v1 → v2)

assocSet :: Parser Substitution
assocSet = do
    sv <- lexeme setVar
    lexeme (string "->")
    e  <- lexeme expr
    return (sv →→ e)

setComponent :: Parser [Substitution]
setComponent = do
    ms <- commaSep assocSet
    lexeme (char '|')
    return ms

subst :: Parser Substitution
subst = do
    lexeme (char '{')
    ms <- option [] setComponent
    as <- commaSep assocVar
    lexeme (char '}')
    return $ build (ms++as)

substAppl :: Parser Command
substAppl = do
    ss <- many1 (lexeme subst)
    me <- optionMaybe $ lexeme (expr <|> singleSetExpr)
    case me of
        Just e -> return (Apply (foldr compose identity ss) e)
        Nothing -> return (Compose ss)
