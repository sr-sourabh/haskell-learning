import Data.List

{-
1.	Define a function

	largestPower :: Int -> Int -> Int

	such that for a positive number n and a prime number p, 

	largestPower n p

	is the largest power of p that divides n! (factorial of n).

	Examples:

	largestPower 10 5 = 2
	largestPower 10 2 = 8
-}

factorial n 
	| n < 0 = -1
	| n == 0 = 1
	| otherwise = n * factorial (n-1)
	
largestpower n p po  k 
	| ((factorial n) `mod` p) /= 0 = k-1
	| otherwise = largestpower n (po^(k+1)) po (k+1)
	
largestPower n p = largestpower n p p 1


{-
2.	Define a function

	nohundred :: Int -> Int

	such that for a positive number n 

	nohundred n

	is the nth positive number such that "100" does not occur as a substring in its binary expansion.

	Examples:

	nohundred 10 = 14
	nohundred 100 = 367
-}

tobinhelp 0 = []		-- to help calculate binary representation of a number
tobinhelp n 
	| n `mod` 2 == 1 = 1:tobinhelp (n `div` 2) 
	| n `mod` 2 == 0 = 0:tobinhelp (n `div` 2) 
	
binary n = reverse(tobinhelp n) 	-- reverse the result returned by helper

nohundredhelp n k c
	| c == n = k-1
	| ([1,0,0] `isInfixOf` (binary k) ==False)   = nohundredhelp n (k+1) (c+1)
	| otherwise = nohundredhelp n (k+1) c


nohundred n = nohundredhelp n 1 0

-- END OF QUESTION 2


{-
3.	Define an infinite list

	infList :: [Integer]

	with the following properties. 
		a) The list is in strictly increasing order
		b) The list begins with the number 1
		c) If the list contains the number x, it also contains 2x, 3x and 5x
		d) The list contains no other numbers

	For example the first 20 elements of infList are [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36].

	Define a function

	infListElem :: Int -> Int

	such that

	infListElem n

	returns the nth element of infList.

-}


merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

infList = 1: merge (map (2*) infList) (merge (map (3*) infList) (map (5*) infList) )
	

nodups [] = []	
nodups (x:xs)
	| x `elem` xs = nodups xs
	| otherwise = x:nodups xs
	

l = take 20000 infList
f = nodups l

infListElem n = (sort f)!!n




{-

4.	We consider strings formed using the two characters 'a' and 'b'. We want to list these strings in order, but there is a problem in 
	the built-in order defined by Haskell. 
	It makes "ab" < "b", "aab" < "ab", "aaab" < "aab" etc., so there is no first string, if we want to list them. So we define an 
	alternate order, call it priorTo:

	priorTo s1 s2 = length s1 < length s2 || (length s1 == length s2 && s1 < s2)

	-- A string s is said to be abundant if there are at least two occurrences of "ab" in s. Define a function

	abundant :: Int -> String

	such that

	abundant n

	returns the nth abundant string according to the priorTo order.


	Examples:

	abundant 1 = "abab"
	abundant 2 = "aabab"
	abundant 5 = "ababb"
	abundant 10 = "aababa"
	
	
	abn = ["abab","aabab","abaab","ababa","ababb","abbab","babab","aaabab","aabaab","aababa"]

abundant n = abn!!n
-}


 

combos :: [Char] -> Int -> [String]
combos chars 1 = map (:[]) chars
combos chars n = concatMap (\front -> map (front ++) (combos chars 1)) $ combos chars (n - 1)

allCombos :: [String]
allCombos = concatMap (combos ['a','b']) [1,2..]

abstr :: [String]
abstr = [x|x<-allCombos,(countab x) >1]

countab :: String->Int
countab []=0
countab [x]=0
countab (y:ys)
  |y=='a' && head ys=='b' = countab (tail ys) +1
  |otherwise = countab ys

abundant :: Int -> String
abundant n = abstr!!(n-1)

priorTo::String->String->Bool
priorTo s1 s2 = (length s1 < length s2) || ((length s1 == length s2) && (s1 < s2))



{-

5.	Define a function

	goodPrime :: Int -> Int

	such that 

	goodPrime n 

	returns the smallest prime number p such that S > n, where S is the sum of the digits of p.

	Examples:

	goodPrime 4 = 5
	goodPrime 10 = 29
	goodPrime 15 = 79

-}
primes = sieve [2..]
	where
		sieve (x:xs) = x:(sieve [y | y <-xs , mod y x > 0])
		
sumofdigits n sum
	| n == 0 = sum
	| otherwise = sumofdigits (n `div` 10) (sum + ( n `mod` 10) )

goodPrimehelp n i
	| (sumofdigits p 0  ) > n = p
	| otherwise = goodPrimehelp n (i+1)
	where
		p = primes!!i
		
goodPrime n = goodPrimehelp n 0



{-

6.	 Conway's look and say sequence is defined as follows:
		– Start off with 1.
		– If you read off the digits, you'd say one 1, so 11 is the next number.
		– Next you'd say two 1s, so 21 is the next.
		– Next up is 1211.
		– Then 111221, 312211, 13112221, 1113213211 etc.

	Define a function

	las :: Int -> Integer

	such that

	las n

	returns the first 4 digits of the nth look-and-see number. (Assume that 1 is the 0th look-and-say number.)

	Examples:

	las 0 = 1
	las 5 = 3122
	las 10 = 1113

-}

convert [] k count = count:k:[]
convert (x:xs) k count
	| k == x = convert xs k (count+1)
	| otherwise = count:k:(convert (x:xs) x 0)



conway = f [[1]] 0
	where 
		f (l:ls) i  = l:f [convert (conway!!i) (conway!!i!!0) 0] (i+1)


converttodecimal [] = 0		
converttodecimal (x:xs) = (10 ^(length (xs))  * x) + converttodecimal xs

las n = converttodecimal ( take 4 (conway!!n) ) 
	
	
	
	
	
	
	
	
	
