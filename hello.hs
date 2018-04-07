-------------------------------------------------------------------------------
--Doing some practice problems, mainly from 
--https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
--Some of these may be a bit longwinded, but it's just for me to understand the
--logic.

lastElement :: [a] -> a
lastElement [x] = x
lastElement (_:xs) = lastElement xs
--Psst, Prelude also provides the function "last"

nextToLastElement :: [a] -> a
nextToLastElement [] = error "No next to last element."
nextToLastElement a = foo (length a) a 0
    where foo length (x:xs) count
            | (length == 1) = error "No next to last element."
            | count == length - 2 = x
            | otherwise = foo length xs (count + 1)
--Cool, here I used a "running count" and compared it to "length", similarly to
--how it would be done in prolog. 

elementAt :: [a] -> Int -> a
elementAt a position = a !! (position - 1)























-------------------------------------------------------------------------------
--Pluralsight Haskell Fundamentals part 1
--https://app.pluralsight.com/player?course=haskell-fundamentals-part1&author=benson-joeris&name=haskell-fundamentals-part1-m6&clip=4&mode=live
--Practiced by Erik 20171030

--VARIABLE DEFINITIONS
string1 = "hello"
string2 = "world"
greeting = string1 ++ " " ++ string2




--FUNCTION DEFINITIONS
--H (Haskell) functions are pure and cannot modify state.
square x = x * x
multMax a b x = (max a b) * x

--posOrNeg "(-3)"" produces "Negative"
posOrNeg x =
    if x >= 0 --no parenthesis around contition
    then "Positive" --no return statements, the entire string is replaced by "Positive"
    else "Negative"




--IMPERATIVE LOOPS TO FUNCTIONAL RECURSION
--No loops in H so use recursion instead! Every loop can be
--rewritten asa  recursive function.
pow2 n = 
    if n == 0
        then 1
        else 2 * (pow2(n-1))

-- returns n copies of str concatenated together.
repeatString str n = 
    if n == 0
        then ""
        else str ++ (repeatString str (n-1))

{- How to systematically translate an imperative function containing
loops to H. Let's say we have a Java function like:

int pow2(int n){
    int x = 1;
    for (int i=0; i<n; ++i)
        x *= 2;
    return x;
}

We can represent it in H like this: notice how we use a helper function 
"pow2loop" to "set extra input parameters" (representing x and i in Java) 
with desired values represen-}
pow2Again n = pow2loop n 1 0
pow2loop n x i =
    if (i<n)
    then pow2loop n (x*2) (i+1) --"update" x and y and loop again. n stays the same.
    else x 


    

--LISTS
--[]
--arbitrary length
--must contain same type.
x = [1,2,3]
empty = []
--prepend an element to a list with ":" aka "cons operator"
--The "cons" operator takes an element and a list and returns a new list with
-- new element added to the front.
y1 = 0 : x -- [0,1,2,3] does not modify x.
--This can be constructed using only : and empty list like so:
y2 = 1 : (2 : (3 : [])) --shortcut: [1,2,3]
--we can actually omit parenthesis in this case...
y3 = 1 : 2 : 3 : [] --wow

{-useful list functions
head
tail
null    tests if a list is empty
-}
double nums =
    if null nums --checks if the nums is empty using the nums function! Cool.
    then [] --"base case" of the recursion
    else (2*(head nums)) : (double (tail nums))

--removes all odd numbers from a list
removeOdd nums =
    if null nums
    then []
    else
        if (mod (head nums) 2) == 0 --even?
        then (head nums) : removeOdd (tail nums) --even
        --when the (head nums) is odd it is not included in the result list,
        --we just removeOdd from the tail of nums recursively
        else removeOdd (tail nums) --odd

--It is possible to write much more compact and readable versions of these functions,
-- even one-liners!



--STRINGS
-- a list of Chars
str = "absde" --is just a shorthand for
str1 = 'a' : 'b' : 'c' : 'd' : 'e' : [] --NOTE the empty list at the end!
--concatenate two lists with ++ (concatenation does not change inputs, creates a new list).
str2 = [1,2,3] ++ [4,5]
-- ++ works for strings, since strings are lists
str3 = "hello " ++ "cruel world"




--TUPLES
--()
--Fixed length
--Can hold many diffrent types, and have more than two elements
x4 = (1, "hello")
y4 = ("pi", 3.14, [1,2,3], "four")

-- lakes a list, returns a tuple with head and length
headAndLength list = (head list, length list)

--fts   access first tuple element
--snd   access second tuple element
y5 = fst (1, "hello")
y6 = snd (1, "hello")

--If tuples get too big and fragile, consider using a custom datatype instead!




--CURRY
--Curried functions are more flexible than functions on tuples, because
--useful functions can often be made by partially applying a curried function.
--If we have
--f :: Int -> Int -> Int -> Int 
--This equals Int -> (Int -> (Int -> Int))
 
--Then calling "f 5" gives the first argument (the leftmost). The result is:
--(f 5) :: Int -> Int -> Int
{- The function type operator (->) associates to the right:
Int -> (Int -> (Int -> Int))

Function application associates to the left: 
(((f 3) 2 ) 1) 
equals 
f 3 2 1

As a consequence, it is natural for function application to associate to the left:
mult x y z
means
(((mult x) y ) z)

Unless tupling is specifically required, all functions in H are normally defined in
curried form. 
-}





--PATTERN MATCHING
--Allows access to data and composite data structures like lists and tuples.

--Takes a tuple and returns the first element, using pattern matching.
--Instead of a variable name, we put something that looks like a tuple
-- with variable names for the pieces of the tuple (a,b) like so:
--a gets binded to first a and b to the b.
fst' (a,b) = a

snd' (a,b) = b

--How the null function which tests if a list is empty is implemented:
null' [] = True
null' (x : xs) = False
--there are two definitions here of the same function. The second def 
--pattern matches any list that is formed by using the : operator to 
--add some x in front of some list xs. 

--How about head function?
head' (x : xs) = x --has a head
head' [] = error "head of empty list" --what's the head of an empty list? An error.

--We can avoid head and tail and do everything with pattern matching.


--multiplies every element in a list by 2, from before
double' nums =
    if null nums
    then []
    else (2 *(head nums)) : (double (tail nums))
--Implementing the above function with patten matching:
double'' (x:xs) = (2 * x) : (double'' xs)
double'' [] = []



--GUARDS AND PATTERN MATCHING
-- = has been moved after the pipes and guarding Boolean expression
pow2' n
    | n == 0    = 1
    | otherwise = 2 * (pow2' (n-1)) -- "otherwise" is always true, 
    --a "catch all"



{-Let's reimplement removeOdd with guards and pattern matching
removeOdd nums =
    if null nums
    then []
    else
        if (mod (head nums) 2) == 0 --even?
        then (head nums) : removeOdd (tail nums) --even
        --when the (head nums) is odd it is not included in the result list,
        --we just removeOdd from the tail of nums recursively
        else removeOdd (tail nums) --odd
-}
removeOdd' [] = [] --match empty list
removeOdd' (x:xs) --match non empty list
    | mod x 2 == 0  = x : removeOdd (xs)
    | otherwise     = removeOdd' (xs)--x not included in the result, 
    --omnly the tail is processed, effectively removing x because 
    --it is odd.


--CASE EXPRESSIONS
--Can be used to pattern match at some other place in the function.
--Cannot use guards, usually use IF.

--Another example of the double implementation:
                           --pattern matchin on the argument nume
doubleCaseExpr nums = case nums of 
    []      -> []
    (x:xs)  -> (2*x) : (doubleCaseExpr xs)
    --in this example, the case expression is equivalent to the pattern matching
    --version of this function above

--another case expression example:
                    --pattern matching on a function call to removeOdd.
anyEven nums = case (removeOdd nums) of
    []      -> False
    (x:xs)  -> True




--LET AND WHERE BINDING
--Local variable bindings (unchangeable)
--"bottom up vs top down" approaches.
fancySeven =
    let a = 3
    in 2*a + 1 -- in marks a subexpression where "a" can be used.

fancyNine =
    let x = 4
        y = 5
    in x + y 

numEven nums =
    let evenNums = removeOdd nums
    in length evenNums

--where binding must be associated with a function definition.
fancySeven' = 2 * a + 1
    where a = 3

fancyNine' = x + y
    where   x = 4
            y = 5



--LAZY EVALUATION
intsFrom n = n : (intsFrom (n+1))
ints = intsFrom 1
{-we can evaluate parts if an infinite list.
    take 10 ints
    [1,2,3,4,5,6,7,8,9,10]
Haskell is so lazy that it evaluates just enough to get the answer and no more.


*Main> let evenInts = removeOdd ints
*Main> take 10 evenInts
[2,4,6,8,10,12,14,16,18,20]
-}



--HIGHER ORDER FUNCTIONS
--In H, functions are just another type of value.
--Can accept or return functions.
--Arguments must be given in order.
pass3 f = f 3
add1 x = x + 1
mult2 x = 2 * x 
compose f g x = f (g x)

--There are also functions that build other
--functions at runtime. 
always7 x = 7
always7' = const 7

--Partial application is another way to 
--create functions at runtime. In most imperative
--langs calling a function with too few params
--is an error, but in H, it produces a new function
--for example:
foo x y z = x + y + z
foo_1_2 = foo 1 2 --calls foo with only 2 args!
--the return of that is a function that takes one
-- arg, the z.

--A more general approach to the above pass3 func.
pass x f = f x
pass3' = pass 3
-- The pass3' function can be built by partially
--applying pass function. Since we only supply the
--first arg to pass, pass3' will be a function that
--takes one argument, whih is the last remaining
--argument needed for the pass function.



--OPERATORS
--can be used as functions, use () around them
z3= (+) 5 3

--this makes it easy to pass operators to higher
--order functions.
pass_3_4 f = f 3 4

--we can define new operators, for example for
--adding pairs of numbers we define a custom
-- .+ operator:
--We're useing pattern matching here, as before.
(a,b) .+ (c,d) = (a + c, b + d)

--operators can also be pratially applied just 
--like other functions. Here's a new version of
--add1 from before.
plus1 = (+) 1

--There's also a special syntax for partially applying 
-- operator by supplying either the left or right
--argument
plus1' = (1+) --we need the right arg
plus1'' = (+1) --we need the left arg

--Any operator can be treated like a function, but
--we can also treat any function like an operator.
--Any function taking two arguments can be turned
--into an operator. For example:
--mod 10 2
--10 `mod` 2

double2 = map (2*)



--FILTER
--Creates new list by testing each element with a 
--boolean function and keep those that pass.
notNull xs = not (null xs) 
--this returns a boolean value if called by itself
    --but 
             --func to test the elements in the list,
             --function notNull should return true
             --or false.
                      --The second parameter is the 
                      --list to be filtered.
    --filter notNull ["", "abc", "", "hello", ""]
    --returns ["abc","hello"]

-- removeOdd can be writteen very easily using filter:
isEven x = x `mod`2 == 0
removeOdd3 = filter isEven
--only the first argument to the filter is given, which
--is the function to test elements. This leaves the function
--accepting one argument, the list to be filtered, which is
--the list passed to removeOdd3.

--Combination of map and filter is useful. 
y7 = map snd (filter fst[(True, 1), (False, 7), (True, 11)]) 
              --1. Consider filter fst and the list first
              -- The first element of each pair is a boolean
              --used for the filter test and the second value
              -- is used if it passes the filter. This list is 
              -- filtered using fst to extract the filter
              -- test value from the pairs. This will remove
              --any pairs with false in the first element.
              --it results in [(True, 1), (True, 11)]
    --2. now consider map snd on [(True, 1), (True, 11)]
    --which results in [1,11].    

    --Using higher order functions, we very rarely have to
    --explicitly iterate over lists like we saw before.

--FOLD
--foldr and foldl
--Combines all values in a list. The first argument is an
--accumulator functino that combines (a number for example)
--with a value in the list (also a number in this case).
--foldl (+) 0 [1,2,3,4]
--10

--zip
--zipWith (+) [1,2,3] [1,2,3]
--zipWith3...

--LAMBDA EXPRESSIONS
--For example, used to define the zipWith3 function:
--zipWith3 (\x y z -> x + y + z)[1,2,3] [4,5,6] [7,8,9]

--FUNCTION OPERATORS:
-- . Function composition
-- $ Function application
-- Order in functino composition is right to left, but
--can also be represented as nested functions:
stringLength x = length (show x)

--Keep in mind: When composing functions, both functinos
--must have only one argument.

--($) takes a function and a value and applies the function
--to that value.




--HASKELL TYPE SYSTEM
--Statically typed.
--Types are inferred. Compiler figures out what types must be.
--Types in H indicates the type to other people.
--Using polymorphic functions, we can write a single function
--that handles multiple types.
-- :t in ghci can be used to ask what type somthing has (GHCI)

--Two arrows is how H denotes two parameters:
-- a -> b -> c
-- This makes sence in light of partially applied functions:
-- If we only pass the first argument to a two argument function 
-- this will return a function that takes a second argument and 
--returns a result.
--So the two argument function is just a one argument function
--that returns another one argument function. 
--We can imagine parenthesis:
-- a -> b -> c = a -> (b -> c)



-- Explicit type
--str :: [Char] --type declaration not necessary.
--str = "hello"

--foo :: Int -> Int
-- ...

--Type annotations (:: Int for example )can be used to check
--where the compiles sees what...


-- POLYMORPHISM
--Lets us use the same function with diffrent types!
-- *Main> :t length
-- length :: Foldable t => t a -> Int
-- "a" above is a placehoder for any type and is called the 
-- type variable. Any function that has a type variable in 
--its definition is a polymorphic funtion. (Nothing to do
--with object oriented polymorphism.) 
-- Type variables all start with lower case letter.
-- Concrete types names start with Upper case letter.
empty_list :: [a]
empty_list = []
list_double = 3.2 : empty_list
list_char = 'a' : empty_list

--repeated type variable always represent the same type:
head'' :: [a] -> a
head'' (x:xs) = x
--If we change to
--head'' :: [a] -> b
--this will return another type without any relation to the
--type of the value in the input list.

--TYPE CLASS CONSTRAINTS
--this works for any type:
-- "Num a" is a constraint, a must be a Num type. Type a is 
--standing for has to support a + operator and interpret 0,
-- multiplication, subtraction, perhaps more.
--sum' :: Num a => [a] -> Int
--sum' [] = 0
--sum' (x:xs) = x + length xs

--multiple type class constraints:
showNum :: (Num a, Show a) => [a] -> [Char]
showNum xs = show (sum xs)





-- CUSTOM TYPES
--type synonyms for radability, ignored by compiler.
type Point = (Double, Double)
--type CustomerId = Int

--newtype defines a
-- new type represented by an existing type
-- new type that are not interchangeable.
newtype CustomerId  = MakeCustomerId Int
                        --constructor
--Ok, but how do we get an int back from a CustomerId?
--Use pattern matching:
customerToInt :: CustomerId -> Int
customerToInt (MakeCustomerId i) = i
--Last line says: "Make i equal to whatever that int was that was
--passed into the constructor MakeCustomerId".

--Naturligtvis, vi kan tänka att (MakeCustomerId i) är parametern till customerToInt!


--Records
data Customer = MakeCustomer
    {   customerId  ::  CustomerId
    ,   name        ::  String 
    ,   luckyNumber ::  Int
}
--The above creates functions by the name of the fields, which
-- allows us to get to the record's fields.

--Records ca nbe updated which keeps some of the values from
--the old records and gives new values to some of the values:
--sally = alice {name="Sally", lyckyNumber=7}

--Records are not extensible and we cannot have any kind of 
--hierarchy. Multiple records cannot have the same field names.


--Algebraic data types
data Customer2 = MakeCustomer2 CustomerId String Int
                --Constructor
--To extract data from ADTs, we use pattern matching.
--getCustomerId :: Customer2 -> CustomerId
--getCustomerId :: (Customer2 cust_id _ _ ) = cust_id

--ADTs support more than one argument. 
--x :: (Double, Double, Double)
--data RGB = RGB Double Double Double
--x :: RGB

--ADT can have multiple constructors...
--Here are two constructors, False and True
--data Bool = False | True

data MaybeInt = NoInt | JustInt Int

--We can use pattern matching to find out whether a value exists
--and extraxt it if it does. This func takes 2 args:
-- 1.   an int to be used a default value if no value is present
--      inside the MaybeInt.
-- 2.   A MaybeInt value (it returns the value inside unless there
--      is none, in which case it returns the defaultValue.)
defaultInt :: Int -> MaybeInt -> Int
defaultInt defaultValue NoInt = defaultValue
defaultInt _ (JustInt x) = x
-- Main> defaultInt 33 (JustInt 3)
-- 3

--We're using an underscore to ignore the defaultValue (when
--JustInt x constructor matches the pattern.)

-- TYPE CLASSES and Type Class Instances
--Consider this function where we use the == to test for equality.
--In order to work for any type that has an equality test, the type 
-- uses the typeclass constraint Eq a:
elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 x (y:ys)
    | x == y = True
    | otherwise = elem1 x ys
--But what if we want to use a custom type?
--We need an instance of the RGB that tells the compiler how to
--compare RGBs for equality:
instance Eq RGB where
    (RGB r1 g1 b1) == (RGB r2 g2 b2) = 
        (r1 == r2) && (g1 == g2) && (b1 == b2)
--This instance uses pattern matching to say that "One RGB is == to
--another RGB if the red, green and blue components of them are 
--equal".

data RGB = RGB Int Int Int
colors = [RGB 255 0 0, RGB 0 255 0, RGB 0 0 255]
green = RGB 0 255 0
greenInColors = elem1 green colors

--Now let's say we want to represent RGB values as a String.
instance Show RGB where --instance
    show (RGB r g b) =  --then implement any functions that the class requires.
        "RGB " ++ (show r) ++ " " ++ (show g) ++
        " " ++ (show b) 
--In above, pattern matching is used.
--Outputting the constructor syntax like the above is how we would 
--normally implement show in a H. func. 




--Type Class Instances for Parameterized Types
data Maybe' a = Nothing' | Just' a
--What if we want a type class instance for Maybe'? 
--Well, it is a parameterized type, so things are different.
--Just like when writing polymorphic functions, we need to
--constring a to be an Eq type:
instance (Eq a) => Eq (Maybe' a) where
    Nothing' == Nothing' = True
    Nothing' == (Just' _) = False
    (Just' _) == Nothing' = False
    (Just' x) == (Just' y) = x == y
--"(Eq a) =>" is the context of the type class instance.


{- DEFINING TYPE CLASSES
When we define an instance of this class (similarly to above)
we would have to define all of its functions. 
It is possible to define some default implementations in the
type class. For Eq, the "minimum complete definition" is 
either == or /= function definition.

Let's say we want a function (or operator) with different 
behavior for each type (overloading): -}
data Point2 = Point2 Double Double --2d point
data Point3 = Point3 Double Double Double --3d point
{-We want to compute the distance between two points of the same
type, with different formulas. To do this, we create a new 
typeclass:-}

class Measurable a where
    distance :: a -> a -> Double

--And a couple of instances of the Measurable type class:
instance Measurable Point2 where
    distance = distance2

instance Measurable Point3 where
    distance = distance3

--Which references these distance functions:
distance2 :: Point2 -> Point2 -> Double
distance2 (Point2 x1 y1) (Point2 x2 y2)=
    sqrt(dx * dx + dy * dy)
    where   dx = x1 - x1
            dy = y1 - y2

distance3 :: Point3 -> Point3 -> Double
distance3 (Point3 x1 y1 z1) (Point3 x2 y2 z2)=
    sqrt(dx * dx + dy * dy + dz * dz)
    where   dx = x1 - x2
            dy = y1 - y2
            dz = z1 - z2
{-Which yields:
 *Main> distance (Point2 3.4 4.5) (Point2 6.7 7.8)
 3.3
 *Main> distance (Point3 3.4 4.5 6.7) (Point3 6.7 7.8 9.9)
 5.658621740318043

Amazing.-}




--SUBCLASSES OF TYPE CLASSES
--Every Type of Ord must also be an Eq. (For in order to order,
--we must implicitly be able to compare equality.) 
--Ord is a subclass of Eq = Eq is a superclass of Ord.




--It's possible to write functions without params:
quadruple = double . double
--is a totally valid composition.

--Factorial
fact n
    | n == 0 = 1
    | n == 1 = 1
    | n > 1 = n * fact(n - 1)

--Fibbonacci


--Collatz
--Returnerar en lista som slutar vid den första ettan.
--Om n är udda så är nästa tal 3*n+1, om n är jämnt
--så är nästa tal n `div` 2.
collatzList :: Integer -> [Integer]
collatzList 1 = [1]
collatzList n = n:collatzList (collatzStep n)
    where collatzStep n
            | odd n     = 3*n+1
            | otherwise = n `div` 2


{- Läser ett tal från stdin och skriver ut listan av Collatziterationer-}
main::IO()
main = do
    line <- getLine
    mapM_ print (collatzList (read line::Integer))

--A naive fib 
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-We can remove the use of lists and get something that
runs in O(n), faster. Let's not hold all the fib numbers
from 0 to n in a list.

If we have a tripple that looks like:
    (n, fibonacci[n-1], fibonacci[n])

Remembering the initial definition, we can calcualte the
next tripple from the last tripple:
    (n+1, fibonacci[n], fibionacci[n-1] + fibonacci[n])
    = (n+1, fibonacci[n], fibonacci[n+1])

And the next from the last tripple: -}






