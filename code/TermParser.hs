module TermParser where

import Control.Applicative

-- More imports as you need.

import TermLib
import FuncLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Term)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser termParser inp
    return ans

-- Master, read heading spaces, parse a block, check whether there is garbage at the end
termParser :: Parser Term 
termParser = whitespaces *> blockParser <* eof

-- Parser for block, read one of cond, lambda, let and infix
blockParser :: Parser Term 
blockParser = condParser <|> lambdaParser <|> letParser <|> infixParser

-- Parser for cond, read a "if", a block, a "then", a block, a "else", and a block in order
condParser :: Parser Term
condParser = fmap (\x y z -> (Cond x y z)) (terminal "if" *> blockParser)
                                            <*> (terminal "then" *> blockParser) 
                                            <*> (terminal "else" *> blockParser)

-- Parser for lambda, read a "\", a var, a "->", and a block in order
lambdaParser :: Parser Term
lambdaParser = fmap (\(Var v) body -> (Lambda v body))
                    (terminal "\\" *> varParser) <*> (terminal "->" *> blockParser)

-- Parser for let, read a "let", a "{", 0 or more equations, a "}", a "in", and a block in order
letParser :: Parser Term
letParser = fmap (\lst body -> (Let lst body))
                 (terminal "let" *> terminal "{" *> many equationParser) 
                  <*> (terminal "}" *> terminal "in" *> blockParser)

-- Parser for equation, read a var, a "=", a block, a ";" in order
equationParser :: Parser (String,Term)
equationParser = fmap (\(Var v) body -> (v,body)) (varParser <* terminal "=") <*> (blockParser <* terminal ";")

-- Parser for infix, read at least one test, associate them to the right according to boolop
infixParser :: Parser Term
infixParser = chainr1 testParser boolcons
  where
    boolcons = fmap (\op -> (\x y -> (Prim2 op x y))) boolopParser

-- Parser for boolop, read a "&&" or a "||"
boolopParser :: Parser String
boolopParser = terminal "||" <|> terminal "&&"

-- Parser for test, read one arith, or one arith, one cmp and one arith
testParser :: Parser Term
testParser = (fmap (\x op y -> (Prim2 op x y)) arithParser <*> cmpParser <*> arithParser) <|> arithParser

-- Parser for cmp, read a "==", a "/=", a "<", or a "<="
cmpParser :: Parser String
cmpParser = terminal "==" <|> terminal "/=" <|> terminal "<=" <|> terminal "<"

-- Parser for arith, read at least one addend, associate them to the left accordinblockParserg to addop
arithParser :: Parser Term
arithParser = chainl1 addendParser addcons
  where
    addcons = fmap (\op -> (\x y -> (Prim2 op x y))) addopParser

-- Parser for addop, read a "+" or a "-"
addopParser :: Parser String
addopParser = terminal "+" <|> terminal "-"

-- Parser for addend, read at least one factor, associate them to the left according to mulop
addendParser :: Parser Term
addendParser = chainl1 factorParser mulcons
  where
    mulcons = fmap (\op -> (\x y -> (Prim2 op x y))) mulopParser

-- Parser for mulop, read a "*" or a "/"
mulopParser :: Parser String
mulopParser = terminal "*" <|> terminal "/"

-- Parser for factor, read at least one atom, associate them to the left
factorParser :: Parser Term
factorParser = chainl1 atomParser (return (\x y -> (App x y)))

-- Parser for atom, read a block with brackets, a literal or a var
atomParser :: Parser Term
atomParser = (between (terminal "(") (terminal ")") blockParser) <|> literalParser <|> varParser 

-- Parser for literal, read either a boolean or an integer
literalParser :: Parser Term
literalParser = (fmap (\x -> (Num x)) integer) <|> booleanParser

-- Parser for boolean, read a "True" or "False" and return Parser (Bln True) or  Parser (Bln False)
-- accordingly and skip trailing spaces
booleanParser :: Parser Term
booleanParser = fmap boolToString (terminal "True" <|> terminal "False")
  where
    boolToString str = case str of
      "True" -> (Bln True)
      "False" -> (Bln False)

-- The reserved words for var
reservedWords :: [String]
reservedWords = ["if", "then", "else", "let", "in", "True", "False"]

-- Read an var and skip trailing spaces
varParser :: Parser Term
varParser = fmap (\str -> (Var str)) (identifier reservedWords)


