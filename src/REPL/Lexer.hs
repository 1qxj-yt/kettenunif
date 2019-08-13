module REPL.Lexer
    ( brackets
    , commaSep
    , natural
    , lexeme
    , whiteSpace
    ) where

import Text.Parsec

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.Functor.Identity (Identity)


lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef String () Identity
style = Lang.emptyDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = Tok.opLetter style
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedOpNames = []
  , Tok.reservedNames   = []
  , Tok.caseSensitive   = True
  }


brackets = Tok.brackets lexer
commaSep = Tok.commaSep lexer
natural = Tok.natural lexer
lexeme = Tok.lexeme lexer
whiteSpace = Tok.whiteSpace lexer
