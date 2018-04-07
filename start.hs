import Data.List
import System.IO

-- Integer unbounded whole number

--Float single precision floating point
--bigFloat = 3.99999999999 + 0.000000000005

--Double precision floating point, up to 11 decimals.

--Tuple can store lists of many diffrent data types.

--Declare a permanent value:
always5 :: Int
always5 = 5

--Using a list.
sumOfNums = sum[1..1000]

addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

--using a prefix operator, the operator comes before the operands:
modExPrefix = mod 5 4
--Using a infix operator:
modExInfix = 5 `mod` 4

negNumEx = 5 + (-4)

-- num9 is of type Int explicitly.
num9 = 9 :: Int

--Use sqrt with num9, but convert num9 to a float
--fromIntegral to convert from Int to Float
sqrtOf9 = sqrt( fromIntegral num9 )

{-A basic function example with parameters.
Multiplies a number by 2 but only if that number is 
smaller than 100 -}
doubleSmallNumber x = if x > 100
    then x
    else x*2
{-The else part is mandatory, every expression and
function must return something. 

An IF statement is an expression.

If we wanted to add one to every number that's produced 
in our previous function, we could have written its 
body like this:
-}
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1  

{-A function that doesn't take any parameters is a
definition. 

Concatenate lists with ++. In a [1,2,3] ++ [4] situation,
Haskell has to walk through the whole list on the left 
side of ++. putting something at the beginning of a list 
using the : operator (also called the cons operator) 
is instantaneous. 
Notice how : takes a number and a list of numbers or 
a character and a list of characters, whereas 
++ takes two lists. Even if you're adding an 
element to the end of a list with ++, you have 
to surround it with square brackets so it becomes 
a list.

[1,2,3] is actually just syntactic sugar for 1:2:3:[]. 
[] is an empty list. If we prepend 3 to it, it 
becomes [3].

Lists can contain lists (of same type).

Lists can be compared in lexicographical order using 
<, <=, > and >= between lists.

Useful functions on lists, usually used in the form:
<function> <list to operate on>

head = the first element
tail = everything except the head
last = last element
init = everything except the last element.

The head of an empty list is a runtime error.

length
null = checks if a list is empty
reverse
take = takes the first number of elements from the list
drop = drops a number of elems from beginning of list
maximum
minimum
sum
product
elem =  takes a thing and a list of things and tells us
if that thing is an element of the list. 

-}
aSmallCat = 'A':" SMALL CAT"

--Int range: -2^63 to 2^63
maxInt = maxBound :: Int

--Integer unbounded whole number, as big as memory can hold.
--Use doubles for most decimals.
bigFloat = 3.99999999999 + 0.00000000005

--Char ''
--Tuples can store lists of diffrent data types.
always5 :: Int
always5 = 5
sumOfNums = sum[1..1000]
num9 = 9 :: Int
sqrtOf9 = sqrt(fromIntegral num9)
primeNumbers = [3,5,7,11]
morePrimes = primeNumbers ++ [13,17,19,23,29]
favNums = 2 : 7 : 21 : 66 : []
multiList = [[1,2,3],[4,5,6]]
morePrimes2 = 2 : morePrimes
lenPrime = length morePrimes2
revPrime = reverse morePrimes2
isListEmpty = null morePrimes2
secondPrime = morePrimes2 !! 1 --access element at position 1
firstPrime = head morePrimes2
lastPrime = tail morePrimes2

--init to get everything except the last element
primeInit = init morePrimes2

first3Primes = take 3 morePrimes2

removedPrimes = drop 3 morePrimes2

is7InList = elem 7 morePrimes2

{-Other useful functions for lists: 
maximum
minimum
sum
product
-}
--making lists:
zeroToTen = [0..10]
evenList = [2,4..20]
letterList = ['A','C'..'Z']
--infinite lists, using only what's needed when needed.
--only generated when we need it.
infPow10 = [10,20..]

many2s = take 10 (repeat 2)
many3s = replicate 10 3
cycleList = take 10(cycle [1,2,3,4,5])
--multiply everything in a list by 3, <= 50:
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]

divideBy9and13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [9, 6, 2, 1]

--zip a list (piecewise) with the given operator
differenceOfLists = zipWith (-) [1,2,3,4,5] [6,7,8,9,10] 
--output: [-5,-5,-5,-5,-5] powerful

listBiggerThan5 = filter (>5) morePrimes

--With infinite lists we can use takeWhile some condition.
---similar to a "while loop". Lazy list evaluation at runtime.
evensUpTo20 = takeWhile(<=20)[2,4..]

--foldr/foldl
--foldl applies an operation on each item from left to right
multOfList = foldl (*) 1 [2,3,4,5]

--list comprehensions
pow3List = [3^n | n <- [1..10]]
--using multiple lists
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]
--list comprehensions with predicate
listComprehensionWithPredicate = [x*2 | x <- [1..10], x*2 >= 12] 

{-a comprehension that replaces each odd number greater 
than 10 with "BANG!" and each odd number that's less 
than 10 with "BOOM!". If a number isn't odd, 
we throw it out of our list. -}
--note how "xs" on the left means input param
--boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
--that last part is a predicate. 

--several predicated example, loaded from argument xs.
--an element must satisfy all the predicates to be 
--included in the resulting list
severalPredicates xs = [ x | x <- xs, x /= 13, x /= 15, x /= 19]
--several predicates example, no arguments
severalPredicates2 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

{-we can also draw from several lists. 
When drawing from several lists, comprehensions 
produce all combinations of the given lists and 
then join them by the output function we supply.
A list produced by a comprehension that draws 
from two lists of length 4 will have a length of 16, 
provided we don't filter them. 
If we have two lists, [2,5,10] and [8,10,11] 
and we want to get the products of all the possible 
combinations between numbers in those lists, here's 
what we'd do.-}
multiList2 = [x*y | x <- [2,5,10], y <-[8,10,11]]

{-a list comprehension that combines a list of adjectives 
and a list of nouns-}
nouns = ["hobo","frog","pope"]  
adjectives = ["lazy","grouchy","scheming"]  
adjectiveAndNouns = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns] 


{- Let's say we want a comprehension that replaces each odd 
number greater than 10 with "BANG!" and each odd number that's 
less than 10 with "BOOM!". If a number isn't odd, we throw it 
out of our list. The last part of the comprehension is the 
predicate. The function odd returns True on an odd number 
and False on an even one. The element is included in the list 
only if all the predicates evaluate to True.-}
boomBangs :: [Integer] -> [String]
boomBangs xs = [if x < 10 then "BOOM" else "BANG" | x <- xs, odd x]

{-Our own version of length.
_ means that we don't care what we'll draw from the list anyway 
so instead of writing a variable name that we'll never use, 
we just write _. This function replaces every element of a 
list with 1 and then sums that up. This means that the resulting 
sum will be the length of our list.

-}
length' xs = sum [1 | _ <- xs]

{-The predicate here does all the work. It says that the 
character will be included in the new list only if it's an 
element of the list ['A'..'Z'].-}
removeNonUppercase st = [ x | x <- st, x `elem` ['A'..'Z'] ]


{-
ram :: Int -> Bool
ram n
    |findpair x n `elem` True = True
    |otherwise = False
    where x = [i <- [1..n]]
-}
findpair :: Int -> Int -> [Bool]
findpair x n = [n - x^3 == i^3 | i <- [(x+1)..n]]


{-
powerset :: [a] -> [[a]]
powerset list = concat([help list x | x <-[0..length(list)]])

help :: [a]->Int->[a]
help list n
    |n == 0 = []
    |n == length(list) = list
    |n==1 = [list !! i | i <- [0..(length(list)-1)]]

    -}


multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z 
 {-In ghci we can do 
    "let r = multThree 3 5" which will make a partial function (unprintable)
    then "r 9" will apply 9 to it and produce 135. neat. -}

--take a number and compare it to 100
-- return GT if the number is less than 100
-- return LT if the number is more than 100
--note the special return type for GT or LT.
--notice that it returns a function (too few arguments applied to compare)
--Compare has a type of (Ord a) => a -> (a -> Ordering) and calling it with 100 
--returns a (Num a, Ord a) => a -> Ordering. The additional class constraint 
--sneaks up there because 100 is also part of the Num typeclass.
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100 


--In H, every function takes one parameter and returns a partually applied
--function until we reach a function that returns a value. 
--Functions can take functions as parameters and also return functions.
--This function takes two parameters and something and retunrs something.
--The first param is a function that takes something (a) and retunrns the
--same thing. The second param is something of that type, and the return
--is of that type also. 
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

