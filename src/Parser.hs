module Parser (parseStat, parseExpr) where
import Syntax

import Control.Applicative ((<$>))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as P
import Text.Parsec.String (Parser)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ haskellStyle 
    { P.reservedNames  = [ "skip", "write", "read" 
                        , "if", "then", "else", "while", "do"
                        ]
    , P.reservedOpNames = ["+", "-", "*", "/", "%", 
                        "==", "!=", ">", ">=", "<", "<=", "&&", "||", ":=", ";"] }

natural :: Parser Integer
natural = P.natural lexer

identifier :: Parser String
identifier = P.identifier lexer

parens :: Parser Expr -> Parser Expr
parens = P.parens lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

variable :: Parser Expr
variable = identifier >>= return . Var

term = parens expr
   <|> Num <$> natural
   <|> Var <$> identifier
   <?> "term"

expr = buildExpressionParser table term
    <?> "expression"

table = [ [ binary "%" Mod AssocLeft, binary "*" Times AssocLeft, binary "/" Divide AssocLeft ]
        , [ binary "+" Plus AssocLeft, binary "-" Minus AssocLeft ]
        , [ binary "&&" And AssocLeft, binary "||" Or AssocLeft]
        , [ binary "==" Eqq AssocLeft, binary "!=" Ne AssocLeft
        , binary ">" GR AssocLeft, binary ">=" GE AssocLeft
        , binary "<" LS AssocLeft, binary "<=" LE AssocLeft  ]
        ]

binary name fun assoc = Infix (do { reservedOp name; return fun }) assoc

conditional :: Parser Stat
conditional = do
  reserved "if"
  a <- expr
  reserved "then"
  b <- stat
  reserved "else"
  c <- stat
  return (If a b c)

while :: Parser Stat
while = do
  reserved "while"
  a <- expr
  reserved "do"
  b <- stat
  return (While a b)


skip :: Parser Stat
skip = do 
    reserved "skip"
    return Skip

semi :: Parser Stat
semi = do 
    a <- stat'
    reserved ";"
    b <- stat
    return (Semi a b)

assign :: Parser Stat
assign = do 
    a <- identifier
    reserved ":="
    b <- expr
    return (Assign a b)

reade :: Parser Stat
reade = do 
    reserved "read"
    b <- expr
    return (Read b)

writee :: Parser Stat
writee = do 
    reserved "write"
    b <- expr
    return (Write b)


stat' = try skip <|> try assign
   <|> try reade <|> try writee 
   <|> try while <|> try conditional
   <?> "stat'"

stat = try semi <|> stat' 
    <?> "stat"

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""

parseStat :: String -> Either ParseError Stat
parseStat = parse stat ""
