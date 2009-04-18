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

