Let's call this language Mu. Why not?

> module Mu
> where

I'm going to be defining stuff myself, so I don't want most of the prelude - just the bare minimum needed for my toInt, toBool functions

> import Prelude (Bool(..),(+),String,(-))

I am going to (hopefully, attempt) to implement lambda calculus.
From the wikipedia definition

Lambda expressions are composed of

    variables v1, v2, . . . vn
    the abstraction symbols λ and .
    parentheses ( )

The set of lambda expressions, Λ, can be defined recursively:

   1. If x is a variable, then x ∈ Λ
   2. If x is a variable and M ∈ Λ, then ( λ x . M ) ∈ Λ
   3. If M, N ∈ Λ, then ( M N ) ∈ Λ

Instances of 2 are known as abstractions and instances of 3, applications.[2]

So, some numbers

> zero = (\f -> \x-> x)
> one = (\f -> \x -> f x)

To test the value of these, one can call these 'numbers' with the arguments (+1) and 0:
*Mu> one (+1) 0
1

Right, just define the rest of the single digits.

> two = (\f -> \x -> f (one f x))
> three = (\f -> \x -> f (two f x))

Ooh, there seems to be a pattern here.
Make a generic successor function instead - this gives one more than the argument

> s = (\n -> \f -> \x -> f (n f x))

So let's get on with defining the rest of them.

> four = (\f -> \x -> (s three) f x)
> five = (\f -> \x -> (s four) f x)
> six = (\f -> \x -> (s five) f x)
> seven = (\f -> \x -> (s six) f x)
> eight = (\f -> \x -> (s (s six)) f x) --for fun
> nine = (\f -> \x -> (s eight) f x)

Right, now time to define some operations.
Start with addition.

This will take two numbers and combine them together.

> add = (\m -> \n -> \f -> \x -> m f (n f x))

Equivalent is 

> add' = (\m -> \n -> \f -> \x -> (m s n) f x)

since one applies the successor to n, m times.


Just for testing, I will define a 'toInt' function, which converts the church numeral to an integer. Note I don't use lambda syntax (this is how I'm going to attempt to keep it seperate).

> toInt x = x (1+) 0

And now we can test the add function:
*Mu> toInt (add three four)
7
*Mu> toInt (add seven eight)
15
Seems to be working.

Next operation - multiplication!
This takes two numbers, and adds the first number to itself the second number times.
We can create a function to 'add n' to a number like so

> addTwo = (\f -> \x -> (add two) f x)
> addN = (\n -> \f -> \x -> (add n) f x)

*Mu> toInt ((addN six) four)
10

So using this.

> mult = (\m -> \n -> \f -> \x -> (m (add n) zero) f x)

Note that most of these definitions are perhaps overly-verbose. I will probably continue writing this way however, since it's more clear to me exactly what's happening.
An alternative mult could be written so

> mult' = (\m -> \n -> (m (add n) zero))

*Mu> toInt (mult' three four)
12

Now let's go one further, and define exponentation. (Since I have no idea how to do subtraction, division)

> expt = (\m -> \n -> \f -> \x -> (n (mult m) one) f x)

-----------------------------------------------------------------
<cheat>
-----------------------------------------------------------------
I don't understand pred. Looking it up, it can be defined as follows - ahh?
PRED := λ n f x. n (λ g h. h (g f)) (λ u. x) (λ u. u) 

> pred = (\n -> \f -> \x -> n (\g -> \h -> h(g f)) (\u -> x) (\u -> u))

----------------------------------------------------------------
</cheat>
----------------------------------------------------------------


Time to continue on.

By convention, true and false are defined as follows.

> true = (\x -> \y -> x)
> false = (\x -> \y -> y)

For testing, create a toBool function

> toBool x = x True False

Now for some boolean operators.

Both have to be true, so the first has to return x and so does the second.

For this to return x (true) then p MUST be passed an x - if p is passed y y then it cannot return x and so q MUST be true. Then p has to return an x too, so p must also be true. Any other combination will hence be false.

> and = (\p -> \q -> \x -> \y -> (p (q x y) y))

If p is true, then the result will be an x (the other result doesn't matter)
Otherwise
  if q is true, then it will return x, and so p's 'y' will be an x, and so the result will be x
  if q is false, then it will return y and so p's 'y' will be a y and so the result will be y (false)

> or = (\p -> \q -> \x -> \y -> (p x (q x y)))

Not is equivalent to simple reversing the arguments.

> not = (\p -> \x -> \y -> (p y x))

Now we can make other boolean operators by combining these.

> nor = (\p -> \q -> \x -> \y -> (not (or p q) x y))
> nand = (\p -> \q -> \x -> \y -> (not (and p q) x y))

xor - in a nutshell, true if the arguments differ. Hence (or p q) and (or (not p) (not q)) should suffice.

> xor = (\p -> \q -> \x -> \y -> (and (or p q) (or (not p) (not q))) x y)

the 'iff' operator (real name??!) is the 'not' of xor - it's true when both values are the same

> iff = (\p -> \q -> \x -> \y -> not (xor p q) x y)

Next a basic contditional
Since true is return the first and false is return the second.

> ifthenelse = (\p -> \f -> \g -> p f g)

And some predicates.

Is not zero can be defined like the following (however there is a better way, so I'll call it isnotzero')

> isnotzero' = (\n -> n (or true) false)

since for any value other than zero, the (or true) gets applied, which is true for both true and false. Hence iszero can be defined

> iszero' = (\n -> (not (isnotzero n)))

The better (simpler) way - define iszero first. Since zero takes a function as an argument, but doesn't apply it, but all the other numbers do, define it as follows

> iszero = (\n -> n (\x -> false) true)
> isnotzero = (\n -> (not (iszero n)))


I'm going to define integers in terms of pairs, and pairs will also be useful when defining things like rationals (and perhaps even lists like [a,[b,[c,[d,e]]]])

> pair = (\a -> \b -> \p -> p a b)
> fst = (\x -> x true)
> snd = (\x -> x false)

List time

Start by defining the empty list.
The first element of any list will signify if it's empty or not
The first element will be true if empty

> nil = pair true true
> isnil = (\l -> fst l)

Now some access functions. Head returns the top of the list or returns nil if there isn't a top (is this good behaviour??!). Tail returns the tail of a list.

> head = (\l -> fst (snd l))
> tail = (\l -> snd (snd l))

Cons is like in lisp - constructs a structure with a left half and a right half

> cons = (\a -> \b -> pair false (pair a b))

As it stands I can't do recursion - errors such as "cannot construct the infinite type" are given. To circumvent this, I apparently need to use the function fix.

> fix :: (a -> a) -> a
> fix f = f (fix f)

??WHO KNOWS??


Integers
I want to allow negative numbers too. Solution: represent integers as a pair of naturals - the integer is the value of first - second.
The following were worked out via thinking and scribbling on paper.

> addInt = (\m -> \n -> (pair (add (fst m) (fst n)) (add (snd m) (snd n))))
> subtractInt = (\m -> \n -> (pair (add (fst m) (snd n)) (add (snd m) (fst n))))
> multiplyInt = (\m -> \n -> (pair 
>                           (add 
>                            (mult (fst n) (fst m))
>                            (mult (snd n) (snd m))
>                           )
>                           (add
>                            (mult (fst n) (snd m))
>                            (mult (snd n) (fst m)))))

It would be nice to have a negate function - this simply swaps the pairs.

> negate = (\n -> pair (snd n) (fst n))

Now I will define some 'integers'. I will use german names, just to differentiate between the naturals and the integers (and also because the ℤ symbol comes from the german word Zahlen)

> null = (pair zero zero)
> eins = (pair one zero)
> zwei = (pair two zero)

Oh, there's a pattern

> natToInt = (\n -> pair n zero)

> drei = natToInt three
> vier = natToInt four
> fünf = natToInt five
> sechs = natToInt six
> sieben = natToInt seven
> acht = natToInt eight
> neun = natToInt nine

Define a toInteger function that handles these types

> toInteger p = toInt (fst p) - toInt (snd p)

Oh, I now have subtraction, so could attempt a factorial function!!




total f = y (\tot n -> if n >= 0 then f n + tot (n - 1) else 0)
