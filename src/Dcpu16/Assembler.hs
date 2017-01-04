module Dcpu16.Assembler where

import Dcpu16.Cpu
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Data.Word
import Data.Char
import qualified Control.Applicative as A

import qualified Text.Parsec.Token as Token

type Context = [(String, Int)] -- collect labels & 

type Parser = Parsec String Context

data AValue = AValue Value
            | AValueSym String      -- symbol
            | AValueSymAddr String  -- [symbol]
            | AValueSymAddrPlusLit String Word16 -- [symbol + lit]
            | AValueSymAddrPlusReg String Reg -- [symbol + reg]
            deriving (Show)

data AInstr = AInstr Instr AValue AValue
            | AInstrDat [Word16]
            | AInstrLabel String
            deriving (Show)


id2regTable = 
    [ ("A", RegA)
    , ("B", RegB)
    , ("C", RegC)
    , ("X", RegX)
    , ("Y", RegY)
    , ("Z", RegZ)
    , ("I", RegI)
    , ("J", RegJ)
    , ("PC", RegPC)
    , ("SP", RegSP)
    , ("O", RegEx)
    ]

id2valueTable = 
    [ ("POP", ValuePop)
    , ("PEEK", ValuePeek)
    , ("PUSH", ValuePush)
    , ("PC", ValuePC)
    , ("SP", ValueSP)
    , ("O", ValueO)
    ]

id2instrTable = 
    [ ("SET", Set)
    , ("ADD", Add)
    , ("SUB", Sub)
    , ("MUL", Mul)
    , ("DIV", Div)
    , ("MOD", Mod)
    , ("SHL", Shl)
    , ("SHR", Shr)
    , ("AND", And)
    , ("BOR", Bor)
    , ("XOR", Xor)
    , ("IFE", Ife)
    , ("IFN", Ifn)
    , ("IFG", Ifg)
    , ("IFB", Ifb)
    , ("JSR", Jsr)
    ]

lexer = Token.makeTokenParser langDef
  where 
    langDef = emptyDef 
      { Token.identStart = letter
      , Token.identLetter = alphaNum
      , Token.commentLine = ";"
      , Token.caseSensitive = False
      }

mkContext :: Context
mkContext = []

parseString :: String -> [AInstr]
parseString str = case runParser toplevel mkContext "" str of
  Left e -> error $ show e
  Right t -> t

colon = Token.colon lexer
comma = Token.comma lexer
ident = Token.identifier lexer

toplevel :: Parser [AInstr]
toplevel = Token.whiteSpace lexer *> many stmt <* eof 

datStmt :: Parser AInstr
datStmt = AInstrDat <$> (Token.reserved lexer "dat" *> Token.commaSep1 lexer literal)

name2instr :: String -> Maybe Instr
name2instr name = lookup (map toUpper name) id2instrTable

instrStmt :: Parser AInstr
instrStmt = do
    instr <- ident
    a <- value 
    comma
    b <- value
    maybe (unexpected $ "Unknown instruction " ++ instr)
         (\x -> return $ AInstr x a b) $
         name2instr instr

stmt :: Parser AInstr
stmt = AInstrLabel <$> (colon *> ident)
    <|> datStmt
    <|> instrStmt
    <?> "stmt"

literal :: Parser Word16
literal = fromIntegral <$> (Token.integer lexer)

value :: Parser AValue
value = term False
     <|> Token.brackets lexer (term True)
     <?> "value"

toAddrValue :: AValue -> Maybe AValue
toAddrValue (AValue (ValueReg r)) = Just $ AValue $ ValueAddrReg r
toAddrValue (AValue (ValueLit v)) = Just $ AValue $ ValueAddr v
toAddrValue (AValueSym v) = Just $AValueSymAddr v
toAddrValue _ = Nothing

sumValues :: AValue -> AValue -> Maybe AValue
sumValues (AValue (ValueLit a)) (AValue (ValueLit b)) = Just $ AValue $ ValueLit $ a + b
sumValues _ _ = Nothing

sumAddrValues' :: AValue -> AValue -> Maybe AValue
sumAddrValues' (AValue (ValueLit a)) (AValue (ValueLit b)) = Just $ AValue $ ValueAddr $ a + b
sumAddrValues' (AValue (ValueReg a)) (AValue (ValueLit b)) = Just $ AValue $ ValueAddrRegPlus a b
sumAddrValues' (AValueSym a) (AValue (ValueLit b)) = Just $ AValueSymAddrPlusLit a b
sumAddrValues' (AValueSym a) (AValue (ValueReg b)) = Just $ AValueSymAddrPlusReg a b
sumAddrValues' _ _ = Nothing

sumAddrValues :: AValue -> AValue -> Maybe AValue
sumAddrValues a b = sumAddrValues' a b A.<|> sumAddrValues b a

term :: Bool -> Parser AValue
term addr = do
    values <- sepBy1 atom (Token.reservedOp lexer "+")
    let v = case  values of
                [x] | addr -> toAddrValue x
                [x] -> Just x
                [x, y] | addr -> sumAddrValues x y
                [x, y] -> sumValues x y
                _ -> Nothing

    maybe (unexpected "Unexpected term") (return . id) v

name2value :: String -> AValue
name2value name' =
    case (lookup name id2valueTable, lookup name id2regTable) of
        (Just v, _) -> AValue v
        (Nothing, Just reg) -> AValue $ ValueReg reg
        (Nothing, Nothing) -> AValueSym name
    where name = map toUpper name'

atom :: Parser AValue
atom = (AValue . ValueLit) <$> literal
    <|> name2value <$> ident
    <?> "atom value"