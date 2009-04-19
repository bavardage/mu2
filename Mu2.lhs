Before I start, some imports:

> import qualified Data.Map as Map
> import Data.Maybe
> import Prelude ()
> import UTF8Prelude hiding (succ,pred,and,or,not)
> import Text.ParserCombinators.Parsec hiding (count)
> import Text.ParserCombinators.Parsec.Expr
> import Monad
> import Data.List (intercalate)
> import Debug.Trace

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
>     | Boolean Bool
>     | Let [(Identifier,Term)] Term
>       deriving Show


I've been told off (in #haskell) for making show pretty-print - show should produce valid code - so instead define a pretty-print function

> pp' (Variable i) = i
> pp' (Application t1 t2) = pp' t1++" "++pp' t2
> pp' (Lambda i t) = "(λ "++i++". " ++ pp' t ++ ")"
> pp' (Boolean b) = show b
> pp' (Let (xs) t) = "let " ++ intercalate ", " (map (\(i,t) -> i ++ " = " ++ pp' t) xs) ++ " in \n(" ++ pp' t ++ ")"

I can't just call pp' and get ghci to show the string, since ghci kills unicode, meaning my lambdas look strange (either as guillemots or escape codes). Instead I have imported UTF8Prelude which provides a putStrLn function which suppports unicode.

> pp = putStrLn.pp'


Expressions will be evaluated to a value of type U (Untyped)
The F type is to represent expressions that evaluate to a function - like λ x. x
The B type is used when converting church booleans to haskell booleans for display.
The I type is used when converting church naturals to haskell integers for display.
The Error type is used when something goes wrong - the string should be set to a description of the error.

> data U = F (U -> U) | B Bool | I Integer | Error String
> instance Show U where
>     show (F u) = "function"
>     show (B b) = show b
>     show (I i) = show i
>     show (Error e) = "ERROR: " ++ e


It'll be useful to be able to 'call' Fs with things. The result of calling anything else'll just be an error

> ($$) :: U -> U -> U
> (F f) $$ x = f x
> a $$ b = Error ("Can only call functions\n Was attempting to call: \n " ++ show a ++ "\n with:\n" ++ show b)

> showNatural :: U -> String
> showNatural i@(F _) = case (i $$ oneplus)
>                       of r@(F _) -> show (r $$ (I 0))
>                          _ -> "Not an integer"
>                 where
>                   oneplus = F oneplus'
>                   oneplus' (I x) = (I (x+1))
> showNatural _ = "Not an integer"

> showBoolean i@(F _) = case (i $$ (B True))
>                       of r@(F _) -> show (r $$ (B False))
>                          _ -> "Not a boolean"


> outputNatural = F (\i -> trace (showNatural i) i)
> outputBoolean = F (\i -> trace (showBoolean i) i)


I need a map to keep track of things.

> type Environment = Map.Map Identifier U

Make a 'default' environment that gives the IO functions

> defaultEnvironment = Map.fromList [("putNat", outputNatural),
>                                    ("putBool", outputBoolean)]  :: Environment



> eval :: Term -> U
> eval = evalWith Map.empty


> evalWith :: Term -> Reader Environment U
> evalWith 




This evaluates an expression in the designated environment.

> evalWith :: Environment -> Term -> U

For variables, just look them up in the map, and cry if it goes wrong.

> evalWith env (Variable i) = fromMaybe (Error ("Variable '" ++ i ++ "' doesn't exist")) (Map.lookup i env)

Return a function that adds the idenifier to the map, and then evals the term with the identifier set to the thing said. Since this new map is only used for the body of the lamba, this gives lexical scope.

> evalWith env (Lambda i t) = F $ (\x -> evalWith (Map.insert i x env) t)


For an Application, call 'a' with 't'

> evalWith env (Application a t) = (evalWith env a) $$ (evalWith env t)
> evalWith env (Boolean b) = B b

For a let statement, add all of the definitions to the map and then call the body with this new map.

> evalWith env (Let [] statement) = evalWith env statement
> evalWith env (Let ((i,t):xs) statement) = evalWith (Map.insert i (evalWith env t) env) (Let xs statement)


Prettier versions of the constructors, just to be used in case I want to fiddle with lamba expressions on the command line - not really so needed now I have a parser.

> λ :: Identifier -> Term -> Term
> λ i t = Lambda i t
> (€) :: Term -> Term -> Term
> a € b = Application a b
> v :: Identifier -> Term
> v i = Variable i

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

...and a variable is just an identifier in the right place.

> parseVariable :: Parser Term
> parseVariable = do
>   identifier <- parseIdentifier
>   return $ Variable identifier

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

> parseExpression :: Parser Term
> parseExpression = parseLambda <|> parseLet <|> parseFunctionApplication <|> parseVariable

> run :: String -> U
> run input = case (parse parseExpression "" input) of
>               Left err -> Error ("Didn't parse: " ++ (show err))
>               Right t -> evalWith defaultEnvironment t

> runFile filename = do
>   contents <- readFile filename
>   result <- return $ run contents
>   putStrLn ("----------\n")
>   return result --we actually have to return something 'cos haskell is so lazy

> runtest = runFile "testprogram1.mu2"
