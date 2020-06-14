{- Programming Paradigms Exercise 2
    Jasmine Roebuck
    Oct. 4, 2019 -}

-- Question 1
-- Partial solution adapted from what was covered in tutorial.
-- Map the given boolean function to the list of all permutations of
-- True/False inputs, then test if the resulting list of Booleans
-- is all True.
twoTautology :: ((Bool,Bool) -> Bool) -> Bool
twoTautology f = and (map f allTwoBools)

-- Map functions f and g to the list of all permutations of True/False
-- inputs, then check if the resulting lists of booleans are equal.
twoEquiv :: ((Bool,Bool) -> Bool) -> ((Bool,Bool) -> Bool) -> Bool
twoEquiv f g = ((map f allTwoBools) == (map g allTwoBools))

-- Generates a list of all possible combinations of True/False arguments.
allTwoBools :: [(Bool, Bool)]
allTwoBools = cartProd [True, False] [True, False]

-- Generates a cartesian product of two sets of elements
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

{- taut' :: (Bool, Bool) -> Bool
taut' (_, _) = True

xor' :: (Bool, Bool) -> Bool
xor' (True, True) = True
xor' (True, False) = False
xor' (False, True) = False
xor' (False, False) = True -}


-- Question 2
-- Solution adapted from what was covered in tutorial:
-- For each n starting from 1, calculate 2^(2^n) + 1 and check if it is 
-- prime. The check is done with list comprehension, adding all 
-- divisors of 2^(2^n) + 1 to a list up to 2^(2^n). If the result is an 
-- empty list, return true, else return false.
-- If the conjecture is true, then the program will never find a case
-- where isPrime returns false, and so will never hit a base case in
-- the recursion. The program would eventually run out of memory.
badFermat :: Integer
badFermat = bfHelper 1
    where exp n = 2^(2^n) + 1
          bfHelper n = if isPrime (exp n)
                       then bfHelper (n + 1)
                       else n

isPrime :: Integer -> Bool
isPrime n = case [x | x <- [2..(n - 1)], n `mod` x == 0] of
                [] -> True
                (a:as) -> False


-- Question 3
-- Solution uses the SF data type that was defined in class. The function
-- checks that an input is a positive integer, then runs the Collatz
-- sequence. The Collatz sequence is applied recursively and constructs
-- a list of all numbers generated until the sequence hits 1.
-- If a number had no Collatz Index, then this function would recurse
-- infinitely and the program would run out of memory.
data SF a = SS a | FF
    deriving (Show)

collatzIndex :: Int -> SF [Int]
collatzIndex x | (x < 1) = FF
               | otherwise = SS (collatz x)

collatz :: Int -> [Int]
collatz x | (x == 1) = [1]
          | (x `mod` 2 == 0) = x:(collatz even)
          | otherwise = x:(collatz odd)
    where
        even = x `div` 2
        odd = (3 * x) + 1


-- Question 5
-- Solution for bubble sort adapted from what was covered in tutorial
-- On each pass, check if the list is sorted. If not, do another iteration
-- of bubble sort. 
bsort :: (a -> a -> Bool) -> [a] -> [a]
bsort p xs | isSorted p xs = xs
           | otherwise = bsort p (bubble p xs)

isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted _ [] = True
isSorted _ [x] = True
isSorted p (x:(y:ys)) = 
    case p x y of
        True -> isSorted p (y:ys)
        False -> False

-- Given a comparator, 'bubble' items further up the list depending
-- on if the comparator evaluates to False.
bubble :: (a -> a -> Bool) -> [a] -> [a]
bubble _ [] = []
bubble _ [x] = [x]
bubble p (x:(y:ys)) = 
    case p x y of
        True -> x:(bubble p (y:ys))
        False -> y:(bubble p (x:ys))

-- Solution for quicksort adapted from the textbook 'Learn You a Haskell
-- For a Great Good': http://learnyouahaskell.com/recursion#quick-sort
-- Also help from https://stackoverflow.com/questions/27970850/sort-a-list-in-haskell-according-to-a-predicate
-- Split a list into elements smaller than and bigger than (except using a-list-in-haskell-according-to-a-predicate
-- custom comparator) the first element. Do this recursively until we hit empty lists,
-- then concatenate the results.
qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort _ [] = []
qsort p (x:xs) = 
    let smallerSorted = qsort p (filter (not . p x) xs)
        biggerSorted = qsort p (filter (p x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- Solution for merge sort adapted from 
-- https://stackoverflow.com/questions/941699/merge-sorted-inputs-in-haskell 
-- and https://stackoverflow.com/questions/19074520/how-to-split-a-list-into-two-in-haskell
-- and https://stackoverflow.com/questions/47598965/haskell-merge-sort-implementation
-- Split the lists in half recursively, then merge them and sort by the custom
-- comparator.
msort :: (a -> a -> Bool) -> [a] -> [a]
msort _ [] = []
msort _ [x] = [x]
msort p xs = merge p firstHalfSorted secondHalfSorted
    where firstHalfSorted = msort p (fst halves)
          secondHalfSorted = msort p (snd halves)
          halves = splitAt halfPoint xs
          halfPoint = length xs `div` 2

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge p xs [] = xs
merge p [] ys = ys
merge p (x:xs) (y:ys) = 
    case p x y of
        True -> x:(merge p xs (y:ys))
        False -> y:(merge p (x:xs) ys)


-- Question 7
-- This was adapted from what was covered in lecture. The naive reverse 
-- uses a recursive call and splits off the first element of the list 
-- and concatenates it onto the the end of the (reversed) tail portion 
-- of the list. The reversed tail is evaluated recursively. The fast 
-- reverse defines a shunt function, which takes the first element of a 
-- list and pushes it onto a secondary list, essentially rebuilding the 
-- list in reverse order. This is also done recursively, but we are 
-- manipulating two lists instead of one.
nreverse :: (Ord a) => [a] -> [a]
nreverse [] = []
nreverse (a:as) = (nreverse as) ++ [a]

freverse :: (Ord a) => [a] -> [a]
freverse xs = shunt xs []
    where shunt [] ys = ys
          shunt (x:xs) ys = shunt xs (x:ys)


-- Question 9
-- Six different ways of programming factorial (names borrowed from the 
-- provided link) https://willamette.edu/~fruehr/haskell/evolution.html:

-- 1. Freshman Haskell programmer: using a basic recursive function 
-- with an if-else statement. Set a base case at n = 0, then 
-- recursively call factorial on (n - 1) and multiply it by n.

-- 2. Sophomore Haskell programmer, at MIT: using an anonymous function 
-- and using prefix operators. Still using recursion and checking the 
-- base case of n = 0. The recursive case multiplies n by factorial 
-- (n - 1), but uses '==', '*', and '-' as prefix instead of infix 
-- operators.

-- 3. Junior Haskell programmer: Another simple recursion, but instead 
-- shifts the index upwards by one, i.e. instead of calling the 
-- recursion on n and (n - 1), we are calling it on (n + 1) and n. Also 
-- takes advantage of Haskell's pattern-matching rather than doing the 
-- recursion with an if-else statement.

-- 4. Another junior Haskell programmer: A simple recursive function 
-- very similar to #1, but uses pattern-matching instead of an if-else 
-- statement, much like #3.

-- 5. Senior Haskell programmer: Simplifies the function into one line 
-- using a fold right and list comprehension. Pass the multiply 
-- operator (*) to the foldr, use 1 as the starting accumulator, and 
-- multiply each item in the list [1..n]. The fold will evaluate the 
-- list by replacing the cons operator (:) with the (*).

-- 6. Another senior Haskell programmer: Does the same thing as #5, but 
-- instead uses a fold left. A fold left evaluates the fold starting 
-- from the left (left-associative brackets) whereas a fold right 
-- starts from the right (right-associative brackets). In this case a 
-- fold left does the same thing as a fold right, since the (*) is 
-- associative and the order of performing the operations does not 
-- matter.
