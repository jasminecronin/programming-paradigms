-- Programming Paradigms Exercise 1
-- Jasmine Roebuck
-- Sep 20, 2019

import Prelude hiding (gcd)

-- Question 1
-- Take the average of three integers and return a float
-- Solution modified from https://stackoverflow.com/questions/3275193/whats-the-right-way-to-divide-two-int-values-to-obtain-a-float
avgThree :: Int -> Int -> Int -> Float
avgThree a b c = ((fromIntegral a) + (fromIntegral b) + (fromIntegral c)) / 3.0


-- Question 2
-- Finds the maximum of three integers, and how many times that maximum occurs
maxThree :: Int -> Int -> Int -> (Int, Int)
maxThree a b c = (m, (maxCount m [a, b, c]))
    where m = (maxTwo (maxTwo a b) c)

-- Helper function: which of two integers is bigger
maxTwo :: Int -> Int -> Int
maxTwo a b = if (a > b) then a else b

-- Helper function: how many one number appears in a list of 3 others
-- Modified from https://stackoverflow.com/questions/10398698/haskell-counting-how-many-times-each-distinct-element-in-a-list-occurs
maxCount :: Int -> [Int] -> Int
maxCount a [] = 0
maxCount a (x:xs) | (a == x) = 1 + (maxCount a xs)
                  | otherwise = maxCount a xs


-- Question 3
-- Returns the largest number whose factorial is no greater than the given number
-- Returns 0 if the given number is <= 0
invFac :: Int -> Int
invFac x | (x <= 0) = 0
         | otherwise = facHelp 1 1 x

-- Helper function: check if the factorial is less than or equal to the given number and recurse
-- x = the potential return number, y = factorial of x, z = the given number from invFac
facHelp :: Int -> Int -> Int -> Int
facHelp x y z = if ((y * (x + 1)) <= z) then (facHelp (x + 1) (y * (x + 1)) z) else x


-- Question 4
-- Takes two integers and returns their greatest common denominator via Euclid's algorithm
gcd :: Int -> Int -> Int
gcd a b | (a == 0 && b == 0) = 0 
        | (b == 0) = a -- TODO deal with negative arguments
        | (a < 0 && b < 0) = gcd (-b) (mod (-a) (-b))
        | (a < 0) = gcd b (mod (-a) b)
        | (b < 0) = gcd (-b) (mod a (-b))
        | otherwise = gcd b (mod a b)


-- Question 5
-- Calculates a binomial coefficient
-- Modified from https://stackoverflow.com/questions/6806946/built-in-factorial-function-in-haskell
binom :: Int -> Int -> Int
binom n 0 = 1 -- choose 0 from n items
binom 0 k = 0 -- choose k from 0 items
binom n k = binom (n - 1) (k - 1) * n `div` k


-- Question 6
-- Given a string, produces a new string with replicated characters depending on the character's index
grow :: String -> String
grow [] = []
grow (x:xs) = growing (x:xs) 1

-- Helper function: Given a string and a number n, calls 'duplicated' n times
growing :: String -> Int -> String
growing [x] a = duplicated x a
growing (x:xs) a = ((duplicated x a) ++ (growing xs (a + 1)))

-- Helper function: Given a character and an int n, returns a list of the character duplicated n times
duplicated :: Char -> Int -> String
duplicated x 1 = [x]
duplicated x a = x:(duplicated x (a - 1))

-- Question 7
-- Tests if a list of integers is strictly increasing
instrictorder :: [Int] -> Bool
instrictorder [] = True
instrictorder [x] = True
instrictorder (x:y:xs) | (x >= y) = False
                       | otherwise = instrictorder (y:xs)


-- Question 10
-- Given a positive number, returns a list of all divisors
divisors :: Int -> [Int]
divisors a | (a <= 0) = []
           | otherwise = divideCheck 1 a

-- Helper function: Does the recursion for the divisors function
divideCheck :: Int -> Int -> [Int]
divideCheck a b | (a == b) = [a]
                | ((mod b a) == 0) = a:(divideCheck (1 + a) b)
                | otherwise = divideCheck (1 + a) b