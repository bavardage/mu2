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

Hmm, the last version didn't go so well. I hit a wall, in that I couldn't do recursion, since haskell is typed.

I can't just use haskell's lamba functions - I'll have to build my own.

Lambda expressions are composed of

    variables v1, v2, . . . vn
    the abstraction symbols λ and .
    parentheses ( )

The set of lambda expressions, Λ, can be defined recursively:

   1. If x is a variable, then x ∈ Λ
   2. If x is a variable and M ∈ Λ, then ( λ x . M ) ∈ Λ
   3. If M, N ∈ Λ, then ( M N ) ∈ Λ

Instances of 2 are known as abstractions and instances of 3, applications.[2]

13:11  Gracenotes: bavardage: here's something to throw at it you're done... Application (Cond (Bool False) (Lambda 'x' (Boole True)) (Lambda 'x' 
                   (Application (Variable 'x') (Boole False)))) (Lambda 'x' (Variable 'x'))

equivalent to > (if False then (\x -> True) else (\x -> x False)) (\x -> x)


Using church encoding, zero could be defined as follows:
λ f. λ x. x
and one:
λ f. λ x. f x
two:
λ f. λ x. f f x or λ f. λ x. f one


> type Identifier = String
> data Term = Variable Identifier 
>     | Application Term Term 
>     | Lambda Identifier Term 
>     | Boolean Bool
>     | Let [(Identifier,Term)] Term
>       deriving Show
>     

I've been told off for making show pretty-print, so instead define a pretty-print function

> pp' (Variable i) = i
> pp' (Application t1 t2) = pp' t1++" "++pp' t2
> pp' (Lambda i t) = "(λ "++i++". " ++ pp' t ++ ")"
> pp' (Boolean b) = show b
> pp' (Let (xs) t) = "let " ++ intercalate ", " (map (\(i,t) -> i ++ " = " ++ pp' t) xs) ++ " in \n(" ++ pp' t ++ ")"

OMG that kills unicode lambda when just doing it straight to gchi. Using putStrLn from UTF8Prelude solves this.

> pp = putStrLn.pp'


When evaling expressions, they will be converted to Us.
The I type will only be used when displaying results, so I hope.

> data U = F (U -> U) | B Bool | Error String | I Integer
> instance Show U where
>     show (F u) = "function"
>     show (B b) = show b
>     show (I i) = show i
>     show (Error e) = "ERROR: " ++ e


It'll be useful to be able to 'call' Fs with things. The result of calling anything else'll just be an error

> ($$) :: U -> U -> U
> (F f) $$ x = f x
> a $$ b = Error ("Can only call functions.. - trying to call " ++ show a ++ " with " ++ show b)

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

> eval :: Term -> U
> eval = evalWith Map.empty

This evaluates an expression in the designated environment.

> evalWith :: Environment -> Term -> U

For variables, just look them up in the map, and cry if it goes wrong.

> evalWith env (Variable i) = fromMaybe (Error ("Variable '" ++ i ++ "' doesn't exist")) (Map.lookup i env)

Return a function that adds the idenifier to the map, and then evals the term with the identifier set to the thing said. (This even seems to give scope!?)

> evalWith env (Lambda i t) = F $ (\x -> evalWith (Map.insert i x env) t)

'Call' a with the result of t.

> evalWith env (Application a t) = (evalWith env a) $$ (evalWith env t)

> evalWith env (Boolean b) = B b

> evalWith env (Let [] statement) = evalWith env statement
> evalWith env (Let ((i,t):xs) statement) = evalWith (Map.insert i (evalWith env t) env) (Let xs statement)


A prettier version of Lambda

> λ :: Identifier -> Term -> Term
> λ i t = Lambda i t


And a prettier applicator and variable would be nice too

> (€) :: Term -> Term -> Term
> a € b = Application a b

> v :: Identifier -> Term
> v i = Variable i

--------------------------------
Time to start defining some Terms.
These are effectively only shorthand remember, since they are still terms.

I really don't need these at all. These will soon be in the standard library instead.

1. Naturals

> zero = (λ "f" (λ "x" (v "x")))
> one = (λ "f" (λ "x" ((v "f") € (v "x"))))
> two = (λ "f" (λ "x" ((v "f") € ((one € (v "f") € (v "x"))))))

Oh look a pattern - make a succ function

> succ = (λ "n"
>           (λ "f"
>              (λ "x"
>                 (
>                  (v "f") € ((v "n") € (v "f") € (v "x"))))))

> three = succ € two
> four = succ € three
> five = succ € four
> six = succ € five
> seven = succ € six
> eight = succ € seven
> nine = succ € eight


I'd now like to define some addition and stuff.
Add is the same as applying the second natural to the result of applying the succ function to the first natural. Multiply is adding the first number the second number of times. Exponentation follows on with this.

> addNats = (λ "m"
>              (λ "n"
>                 ((v "m") € succ € (v "n"))))

> multNats = (λ "m"
>               (λ "n"
>                  ((v "m") € (addNats € (v "n")) € zero)))

> exptNats = (λ "m"
>               (λ "n"
>                  ((v "n") € (multNats € (v "m")) € one)))


I'll also just define a standard pred (I don't understand this yet though)
λ n f x. n (λ g h. h (g f)) (λ u. x) (λ u. u) 

> pred = (λ "n"
>           (λ "f"
>              (λ "x"
>                 (v "n" € (λ "g"
>                             (λ "h"
>                              ((v "h") € ((v "g") € (v "f"))))
>                          ) € (λ "u" (v "x")) € (λ "u" (v "u"))))))
>                          


2. Conditionals

I cheated somewhat and haven't got church booleans. TUT TUT TUT. Maybe I should?
I *will* use church booleans, because it makes other conditional stuff easier.

These are pretty much just 'convention'

> true = (λ "x"
>           (λ "y" 
>              (v "x")))
> false = (λ "x"
>            (λ "y"
>               (v "y")))

Now some logic operators.
And has to be true - return an x - iff both arguments are true
Compose p with q with p.
Is left assoc

> and = (λ "p"
>          (λ "q"
>             ((v "p") € (v "q") € (v "p"))))

> iszero = (λ "n"
>             ((v "n") € (λ "x" false) € true))

3. Recursion

This is the y combinator - (it's in messy form because I typed it in in lambda form and parsed it using the parsers below)

> y = Lambda "g" (Application (Lambda "x" (Application (Variable "g") (Application (Variable "x") (Variable "x")))) (Lambda "x" (Application (Variable "g") (Application (Variable "x") (Variable "x")))))

> count' = (λ "f"
>            (λ "n"
>               ((
>                 iszero € (v "n")
>                ) € (
>                     zero
>                    ) € (
>                         addNats € (v "n") € ((v "f") € (pred € (v "n")))))))

> count = (λ "x" (count' € (y € count') € (v "x")))

> fact' = (λ "f"
>           (λ "n"
>              ((
>                iszero € (v "n")
>               ) € (
>                    one
>                   ) € (
>                        multNats € (v "n") € ((v "f") € (pred € (v "n")))))))

> fact = (λ "x" (fact' € (y € fact') € (v "x")))


4. The parser

Really, I'd like to be able to do more than just code stuff within haskell. I need a parser.


An identifier is just a string...

> parseIdentifier :: Parser Identifier
> parseIdentifier = do 
>   many1 letter <?> "identifier"

...and a variable is just an identifier in the right place.

> parseVariable :: Parser Term
> parseVariable = do
>   identifier <- parseIdentifier
>   return $ Variable identifier

A function application is a list of identifiers

> parseFunctionApplication :: Parser Term
> parseFunctionApplication = do
>   char '(' <?> "opening bracket"
>   identifiers <- sepBy1 parseExpression (char ' ' <?> "")
>   char ')' <?> "closing bracket"
>   return $ makeApplications identifiers
>     where
>       makeApplications :: [Term] -> Term
>       makeApplications (a:as) = foldl Application a as

-->       makeApplications [] = error "Cannot apply nothing to nothing"
-->       makeApplications [a] = a
-->       makeApplications as = Application (makeApplications (init as)) (last as)


> parseLambda = do
>   try(string "λ")
>   many (char ' ')
>   identifiers <- sepEndBy1 parseIdentifier (many $ (char ' ' <?> "identifier spacer"))
>   --many (char ' ')
>   char '.'
>   many (char ' ')
>   expr <- parseExpression
>   return $ foldr Lambda expr identifiers

> parseDefinition = do
>   identifier <- parseIdentifier
>   many (char ' ')
>   char '='
>   many (char ' ')
>   term <- parseExpression
>   return (identifier,term)

> parseLet = do
>   try(string "let ")
>   definitions <- sepBy1 parseDefinition (string "," >> many (oneOf "\n ")<?> "")
>   many (oneOf " \n")
>   string "in"
>   many (oneOf " \n")
>   term <- parseExpression
>   return $ Let definitions term

> parseExpression :: Parser Term
> parseExpression = parseLambda <|> parseLet <|> parseFunctionApplication <|> parseVariable


Make a very basic environment - provide some functions

> env = Map.fromList [("putNat", outputNatural),
>                     ("putBool", outputBoolean)]  :: Environment

> run :: String -> U
> run input = case (parse parseExpression "" input) of
>               Left err -> Error ("Didn't parse: " ++ (show err))
>               Right t -> evalWith env t

> runFile filename = do
>   contents <- readFile filename
>   result <- return $ run contents
>   putStrLn ("----------\n")
>   return result --we actually have to return something 'cos haskell is so lazy

> runtest = runFile "testprogram1.mu2"
