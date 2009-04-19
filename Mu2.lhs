This will be called Mu2 
(as in µ2)

My thought process:
 - lamba calc
 -> µs and lambas go together quite often, so I think they must be friends.
 -> make first attempt at language, call it Mu.
 -> that failed
 -> make second attempt
 -> call it µ2
 -> OMG THAT'S A POKEMON!!!


> module Mu2
> where

Before I start, some imports:

> import qualified Data.Map as Map
> import Data.Maybe
> import Prelude ()
> import UTF8Prelude hiding (succ,pred,and,or,not)
> import Text.ParserCombinators.Parsec hiding (count)
> import Text.ParserCombinators.Parsec.Expr
> import Monad
> import Control.Monad.Reader
> import Data.List (intercalate)
> import Debug.Trace
> import Ratio

I want to make myself an implementation of a basic lamba calculus.
I can't just use haskell's lambda functions, since haskell's lambda functions are untyped - I'll have to build my own.

From wikipedia:
Lambda expressions are composed of

    variables v1, v2, . . . vn
    the abstraction symbols λ and .
    parentheses ( )

The set of lambda expressions, Λ, can be defined recursively:

   1. If x is a variable, then x ∈ Λ
   2. If x is a variable and M ∈ Λ, then ( λ x . M ) ∈ Λ
   3. If M, N ∈ Λ, then ( M N ) ∈ Λ

I will try and keep to this as much as possible, but if I deviate a little, so what?


> type Identifier = String
> data Term = Variable Identifier 
>     | Application Term Term 
>     | Lambda Identifier Term 
>     | Let [(Identifier,Term)] Term
>       deriving Show


I've been told off (in #haskell) for making show pretty-print - show should produce valid code - so instead define a pretty-print function

> pp' (Variable i) = i
> pp' (Application t1 t2) = pp' t1++" "++pp' t2
> pp' (Lambda i t) = "(λ "++i++". " ++ pp' t ++ ")"
> pp' (Let (xs) t) = "let " ++ intercalate ", " (map (\(i,t) -> i ++ " = " ++ pp' t) xs) ++ " in \n(" ++ pp' t ++ ")"

I can't just call pp' and get ghci to show the string, since ghci kills unicode, meaning my lambdas look strange (either as guillemots or escape codes). Instead I have imported UTF8Prelude which provides a putStrLn function which suppports unicode.

> pp = putStrLn.pp'


Expressions will be evaluated to a value of type U (Untyped)
The F type is to represent expressions that evaluate to a function - like λ x. x
The B type is used when converting church booleans to haskell booleans for display.
The I type is used when converting church naturals to haskell integers for display.
The Error type is used when something goes wrong - the string should be set to a description of the error.

> data U = F (U -> U) | B Bool | I Integer | R Rational | C Rational Rational | Error String
> instance Show U where
>     show (F u) = "function"
>     show (B b) = show b
>     show (I i) = show i
>     show (R r) = show r
>     show (C r i) = "("++show r++") + ("++show i++")i"
>     show (Error e) = "ERROR: " ++ e


It'll be useful to be able to 'call' Fs with things. The result of calling anything else'll just be an error

> ($$) :: U -> U -> U
> (F f) $$ x = f x
> a $$ b = Error ("Can only call functions\n Was attempting to call: \n " ++ show a ++ "\n with:\n" ++ show b)

> first =  run "λ x. (x (λ x y. x))"
> second = run "λ x. (x (λ x y. y))"
> getNatural :: U -> U
> getNatural i@(F _) = case (i $$ oneplus)
>                      of r@(F _) -> r $$ (I 0)
>                         (Error e) -> Error e
>                         _ ->  Error "not a natural"
>     where
>       oneplus = F oneplus'
>       oneplus' (I x) = (I (x+1))
> getNatural (Error e) = Error e
> getNatural _ = Error "not a natural"

> showNatural :: U -> String
> showNatural = show.getNatural

> showBoolean i@(F _) = case (i $$ (B True))
>                       of r@(F _) -> show (r $$ (B False))
>                          _ -> "Not a boolean"


> getInteger :: U -> U
> getInteger i@(F _) = case getNatural (first $$ i)
>                      of (I a) -> case getNatural (second $$ i)
>                                 of (I b) -> I (a - b)
>                                    _ -> Error "not an integer"
>                         (Error e) -> Error e
>                         _ -> Error "not an integer"
> getInteger e@(Error _) = e
> getInteger _ = Error "not an integer"

> showInteger = show.getInteger

> getRational :: U -> U
> getRational r@(F _) = case getInteger (first $$ r)
>                       of (I a) -> case getInteger (second $$ r)
>                                  of (I b) -> R (a % b)
>                                     (Error e) -> Error e
>                                     _ -> Error "not a rational"
>                          (Error e) -> Error e
>                          _ -> Error "not a rational"
> getRational (Error e) = Error e
> getRational _ = Error "not a rational"

> showRational = show.getRational

> getComplex :: U -> U
> getComplex c@(F _) = case getRational (first $$ c)
>                       of (R r) -> case getRational (second $$ c)
>                                  of (R i) -> C r i
>                                     (Error e) -> Error e
>                                     _ -> Error "not a complex number"
>                          (Error e) -> Error e
>                          _ -> Error "not a complex number"
> getComplex (Error e) = Error e
> getComplex _ = Error "not a complex number"

> showComplex = show.getComplex

> outputNatural = F (\n -> trace (showNatural n) n)
> outputBoolean = F (\b -> trace (showBoolean b) b)
> outputInteger = F (\i -> trace (showInteger i) i)
> outputRational = F (\r -> trace (showRational r) r)
> outputComplex = F (\c -> trace (showComplex c) c)


I need a map to keep track of things.

> type Environment = Map.Map Identifier U

Make a 'default' environment that gives the IO functions

> defaultEnvironment = Map.fromList [("putNat", outputNatural),
>                                    ("putInt", outputInteger),
>                                    ("putBool", outputBoolean),
>                                    ("putRational", outputRational),
>                                    ("putComplex", outputComplex)]  :: Environment

> eval :: Term -> U
> eval term = runReader (evalWith term) defaultEnvironment

> evalWith :: Term -> Reader Environment U
> evalWith (Variable i) = do
>   result <- asks (Map.lookup i)
>   return $ fromMaybe (Error ("var " ++ i ++ " doesn't exist")) result

> evalWith (Lambda i t) = do
>   env <- ask
>   let func = (\x -> runReader (local (Map.insert i x) (evalWith t)) env)
>   return $ F func

> evalWith (Application a t) = do
>   a' <- evalWith a 
>   b' <- evalWith t
>   return $ a' $$ b'

> evalWith (Let [] statement) = evalWith statement
> evalWith (Let ((i,t):xs) statement) = do
>   term <- evalWith t
>   local (Map.insert i term) (evalWith (Let xs statement))


Prettier versions of the constructors, just to be used in case I want to fiddle with lamba expressions on the command line - not really so needed now I have a parser.

> λ :: Identifier -> Term -> Term
> λ i t = Lambda i t
> (€) :: Term -> Term -> Term
> a € b = Application a b
> v :: Identifier -> Term
> v i = Variable i

----------------------------
| Parser Utility Functions |
----------------------------


> zero = "λ f x. x"
> succ = "λ n f x. (f (n f x))"

> churchifyNat :: Integer -> Term
> churchifyNat i = case parse parseExpression "" (churchifyNat' i)
>                  of Left err -> error $ show err
>                     Right t -> t
> churchifyNat' :: Integer -> String
> churchifyNat' 0 = "(" ++ zero ++")"
> churchifyNat' n = "(("++succ++") "++churchifyNat' (n-1)++")"

> churchifyInt n = case parse parseExpression "" (churchifyInt' n)
>                  of Left err -> error $ show err
>                     Right t -> t
> churchifyInt' n = "((λ a b f. (f a b)) "++churchifyNat' n++zero++")"

--------------
| The parser |
--------------
Whitespace is a space or a newline

> whitespace :: Parser ()
> whitespace = do
>   many (oneOf " \n\t" <?> "whitespace")
>   return ()

An identifier is just a string...

> parseIdentifier :: Parser Identifier
> parseIdentifier = do 
>   many1 letter <?> "identifier"

> parseNumber :: Parser Term
> parseNumber = do
>   digits <- many1 digit
>   let number = read digits
>   identifier <- oneOf "ni" <?> "type identifier (n,i)"
>   return $ case identifier of
>              'n' -> churchifyNat number
>              'i' -> churchifyInt number


...and a variable is just an identifier in the right place OR a number

> parseVariable :: Parser Term
> parseVariable = do
>   liftM Variable parseIdentifier <|> parseNumber

A function application is a list of identifiers wrapped in brackets.

> parseFunctionApplication :: Parser Term
> parseFunctionApplication = do
>   char '(' <?> "opening bracket"
>   identifiers <- sepBy1 parseExpression whitespace
>   char ')' <?> "closing bracket"
>   return $ makeApplications identifiers
>     where
>       makeApplications :: [Term] -> Term
>       makeApplications (a:as) = foldl Application a as

A lambda has the form λ a b c. <expression>

> parseLambda = do
>   try(string "λ")
>   whitespace
>   identifiers <- sepEndBy1 parseIdentifier whitespace
>   char '.'
>   whitespace
>   expr <- parseExpression
>   return $ foldr Lambda expr identifiers

A definition is <identifier> = <expression>

> parseDefinition = do
>   identifier <- parseIdentifier
>   whitespace
>   char '='
>   whitespace
>   term <- parseExpression
>   return (identifier,term)

> parseLet = do
>   try(string "let ")
>   definitions <- sepBy1 parseDefinition (string "," >> whitespace <?> "")
>   whitespace
>   string "in"
>   whitespace
>   term <- parseExpression
>   return $ Let definitions term

> parseExpression :: Parser  Term
> parseExpression = parseLambda <|> parseLet <|> parseFunctionApplication <|> parseVariable

> run :: String -> U
> run input = case (parse parseExpression "" input) of
>               Left err -> Error ("Didn't parse: " ++ (show err))
>               Right t -> eval t

> runFile filename = do
>   stdlib <- readFile "stdlib.mu2"
>   contents <- readFile filename
>   result <- return $ run (stdlib ++ contents)
>   putStrLn ("----------\n")
>   return result --we actually have to return something 'cos haskell is so lazy

> runtest = runFile "testprogram1.mu2"
