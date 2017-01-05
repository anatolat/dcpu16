module Dcpu16.Assembler.Parser 
    ( parseString
    , parseFile
    ) where

import Dcpu16.Cpu
import Dcpu16.Assembler.Syntax
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Data.Word
import Data.Char
import qualified Control.Applicative as A
import qualified Text.Parsec.Token as Token

type Parser = Parsec String ()

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
      { Token.identStart = letter <|> char '_'
      , Token.identLetter = alphaNum  <|> char '_'
      , Token.commentLine = ";"
      , Token.caseSensitive = False
      }

parseString :: String -> [AInstr]
parseString str = case runParser toplevel () "" str of
  Left e -> error $ show e
  Right t -> t

parseFile :: FilePath -> IO [AInstr]
parseFile filePath = parseString <$> readFile filePath  

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
    name <- ident
    instr <- maybe (unexpected $ "Unknown instruction " ++ name) return $ name2instr name
    a <- value 
    b <- case instr of 
        Jsr -> return $ AValue $ ValueLit 0 -- placeholder value
        _ -> comma >> value
    return $ AInstr instr a b
         
stmt :: Parser AInstr
stmt = (AInstrLabel . map toUpper) <$> (colon *> ident)
    <|> datStmt
    <|> instrStmt
    <?> "stmt"

literal :: Parser Word16
literal = fromIntegral <$> Token.integer lexer

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

    maybe (unexpected "Unexpected term") return v

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