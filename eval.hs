{-# LANGUAGE ExistentialQuantification #-}

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
-- import Control.Monad.Except
import Control.Monad.Error
import System.IO
import Data.IORef

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Rational Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

main :: IO ()
main =
  do args <- getArgs
     if null args then runRepl else runOne $ args
     -- case length args of
     --      0 -> runRepl
     --      1 -> runOne $ args !! 0
     --      otherwise -> putStrLn "Program takes only 0 or 1 argument"
-- main = do
--          args <- getArgs
--          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--          putStrLn $ extractValue $ trapError evaled
-- main = getArgs >>= print . eval . readExpr . head

--------------------------------
------------ Parser ------------
--------------------------------

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseComment :: Parser LispVal
parseComment = do
    char ';'
    skipMany (noneOf "\r\n")
    -- manyTill anyChar newline >> spaces
    return $ Bool False
-- parseComment = char ';' >> anyChar >> return $ Bool False
-- escape :: Parser String
-- escape = do
--             d <- char '\\'
--             c <- oneOf "\\\"0nrvtbf"
--             return [d, c]

-- nonEscape :: Parser Char
-- nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

escapedChars :: Parser Char
escapedChars = do 
                 char '\\'
                 x <- oneOf "\\\"nrt"
                 return $ case x of
                   '\\' -> x
                   '"'  -> x
                   'n'  -> '\n'
                   'r'  -> '\r'
                   't'  -> '\t'

-- character :: Parser String
-- character = fmap return nonEscape <|> escape

parseString :: Parser LispVal
parseString = do
                char '"'
                -- strings <- many character
                x <- many $ escapedChars <|> noneOf "\"\\"
                char '"'
                return $ String x
                -- return $ (String . concat) strings

parseNumber :: Parser LispVal
parseNumber =  parseDigital1
           <|> parseDigital2
           <|> parseHex
           <|> parseOct
           <|> parseBin

-- Default digital number
parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= return . Number . read

-- Explicit #d digital
parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d" -- Check for the #d
                   x <- many1 digit  -- Eat the digits
                   (return . Number . read) x

-- Hex number
parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 digit
              return $ Number (hex2dig x)

-- Oct parser
parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 digit
              return $ Number (oct2dig x)

-- Bianary parser
parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 digit
              return $ Number (bin2dig x)

-- Converts oct to decimal
oct2dig :: String -> Integer
oct2dig x = fst $ readOct x !! 0

-- Converts hex to decimal
hex2dig :: String -> Integer
hex2dig x = fst $ readHex x !! 0

-- I don't understand, stolen from http://pleac.sourceforge.net/pleac_haskell/numbers.html#AEN82
bin2dig :: String -> Integer
bin2dig = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

-- Float parser
parseFloat :: Parser LispVal
parseFloat = do
               x <- many1 digit
               char '.'
               y <- many1 digit
               return $ Float (fst . head $ readFloat (x++"."++y))

-- Parse rationals, that is fractions
parseRational :: Parser LispVal
parseRational = do
                  x <- many1 digit
                  char '/'
                  y <- many1 digit
                  return $ Rational ((read x) % (read y))

-- Convert LispVal to double for future re-conversion
toDouble :: LispVal -> Double
toDouble (Float f)  = f
toDouble (Number n) = fromIntegral n

-- Parse complex numbers
parseComplex :: Parser LispVal
parseComplex = do
                 x <- (try parseFloat <|> parseDigital1)
                 char '+'
                 y <- (try parseFloat <|> parseDigital1)
                 char 'i'
                 return $ Complex (toDouble x :+ toDouble y)

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseBool :: Parser LispVal
parseBool = do
              char '#'
              (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

-- Parse character literals
-- Technically not standards complient
parseCharacter :: Parser LispVal
parseCharacter = do
                   try $ string "#\\"
                   value <- try (string "space" <|> string "newline")
                           <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
                   return $ Character $ case value of
                     "space"   -> ' '
                     "newline" -> '\n'
                     otherwise -> (value !! 0)

-- Parses a normal list or a dotted list
parseAnyList :: Parser LispVal
parseAnyList = do
                 char '('
                 x <- try parseList <|> parseDottedList
                 char ')'
                 return x

-- Base List parser
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- Parses vectors contents
parseVector :: Parser LispVal
parseVector = do
                arrayValues <- sepBy parseExpr spaces
                return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)

-- Wrap around the vector parser,
-- There should be a way to consolidate these
parseVectorWrap :: Parser LispVal
parseVectorWrap = do
                    string "#("
                    x <- parseVector
                    char ')'
                    return x

-- Parse dotted lists
parseDottedList :: Parser LispVal
parseDottedList = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList head tail

-- Single-quote syntactic sugar
parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]

-- Quasi Quote black magic
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
                     char '`'
                     x <- parseExpr
                     return $ List [Atom "quasiquote", x]

-- Evaluate when in a quasi quote
parseUnQuote :: Parser LispVal
parseUnQuote = do
                 char ','
                 x <- parseExpr
                 return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr =  parseComment
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> parseAnyList
         <|> parseQuasiQuoted
         <|> parseUnQuote
         -- <|> try parseComment
         <|> try parseVectorWrap
         <|> try parseComplex
         <|> try parseRational
         <|> try parseFloat
         <|> try parseNumber -- All can start with a hash, so
         <|> try parseBool   -- the 'try's are necessary
         <|> try parseCharacter

-- readExpr :: String -> ThrowsError LispVal
-- readExpr input = case parse parseExpr "lisp" input of
--     Left err  -> throwError $ Parser err
--     Right val -> return val

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

--------------------------------
------------- Show -------------
--------------------------------

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Character char)       = "#\\" ++ [char]
showVal (Float contents)       = show contents
showVal (Rational num)         = (show . numerator $ num) ++ "/" ++ (show . denominator $ num)
showVal (Complex num)          = (show . realPart $ num) ++ "+" ++ (show . imagPart $ num) ++ "i"
showVal (Vector arr)           = "#(" ++ (unwordsList $ elems arr) ++ ")"
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)               = "<IO port>"
showVal (IOFunc _)             = "<IO primitive>"

instance Show LispVal where show = showVal

--------------------------------
------------ Errors ------------
--------------------------------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg  = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

--------------------------------
---------- Evaluation ----------
--------------------------------

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             _          -> eval env conseq
             -- Bool True  -> eval env conseq
             -- _          -> throwError $ TypeMismatch "bool" pred
eval env (List (Atom "cond" : expr : rest)) =
     do eval' env expr rest
     where eval' env (List [cond, value]) (x : xs) =
                 do result <- eval env cond
                    case result of
                         Bool True  -> eval' env x xs
                         Bool False -> eval env value
                         otherwise  -> throwError $ TypeMismatch "bool" cond
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List (Atom "define" : List (Atom var : params) : body)) = 
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List (func : args)) = --mapM (eval env) args >>= liftThrows . apply func
  do func <- eval env func
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args
apply func args = throwError $ NotFunction "Unrecognized primitive function args" (show args)
-- apply _ _ = throwError $ Default "apply"
-- apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
--                         ($ args)
--                         (lookup func primitives)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop quot),
              ("modulo", numericBinop quot),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [params] = op params

symbolp, stringp, numberp, boolp, listp :: LispVal -> ThrowsError LispVal
symbolp (Atom _)       = return $ Bool True
symbolp _              = return $ Bool False
numberp (Number _)     = return $ Bool True
numberp _              = return $ Bool False
stringp (String _)     = return $ Bool True
stringp _              = return $ Bool False
boolp   (Bool _)       = return $ Bool True
boolp   _              = return $ Bool False
listp (List _)         = return $ Bool True
listp (DottedList _ _) = return $ Bool True
listp _                = return $ Bool False

symbol2string, string2symbol :: LispVal -> ThrowsError LispVal
symbol2string (Atom a)   = return $ String a
symbol2string _          = return $ String ""
string2symbol (String a) = return $ Atom a
string2symbol _          = return $ Atom ""

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
-- numericBinop op params = Number $ foldl1 op $ map unpackNum params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notStr     = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- unpackNum (String n) = let parsed = reads n in
--                            if null parsed
--                               then throwError $ TypeMismatch "number" $ String n
--                               else return $ fst $ parsed !! 0
-- unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

--------------------------------
-------- List Primitives -------
--------------------------------

-- car - first list entry
-- cdr - all but the first
-- cons - list constructor

-- Returns the first entry in a list (head)
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

-- Returns all but the first entry in a list (tail)
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

-- List constructor, like (1 : 2 : [])
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

-- Strict(ish) equality test, so 2 == 2 and 2 /= "2"
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List arg1), l2@(List arg2)]       = eqvList eqv [l1, l2]
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

-- Helper function for testing recursively though a list
-- Take eqv or equal depending on test, then applies it to each item
-- of both lists (assuming they're they same length)
eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                  Left err         -> False
                                  Right (Bool val) -> val

-- Type for unpacker functions (needs ExistentialQuantification)
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- Takes two LispVals and an Unpacker function. Unpacks both LispVals
-- with the Unpacker function and compares the two with Haskell.
-- Errors return false (eg 2 == '2')
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

-- Less strict equality test, so 2 == '2'
-- Tests each argument with the Unpackers and with eqv.
equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List arg1), l2@(List arg2)]       = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] =
  do primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
     eqvEquals <- eqv [arg1, arg2]
     return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

--------------------------------
--------- Conditionals ---------
--------------------------------

-- For (cond) see eval

--------------------------------
------------- REPL -------------
--------------------------------

-- Prints a string then immediately flushes the buffer
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Prints a prompt and reads a line
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Evaluate a string, that is, use readExpr to get a LispVal then it >>= into eval
-- (due to ThrowError), then apply show on it. Extract and return the result (inc. errors)
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
-- evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

-- Evalulate and print a string
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
  do result <- prompt
     if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args =
  do env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
     (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr
-- runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
-- runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

--------------------------------
--------- Environments ---------
--------------------------------

-- Mutable IORef variables
type Env = IORef [(String, IORef LispVal)]

-- Error handling IO monad
type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =
  do env <- liftIO $ readIORef envRef
     maybe (throwError $ UnboundVar "Getting an unbound variable" var)
           (liftIO . readIORef)
           (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
  do env <- liftIO $ readIORef envRef
     maybe (throwError $ UnboundVar "Setting an unbound variable" var)
           (liftIO . (flip writeIORef value))
           (lookup var env)
     return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value =
  do alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env  = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

--------------------------------
----------- Functions ----------
--------------------------------

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

--------------------------------
------------- Ports ------------
--------------------------------

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdin]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename