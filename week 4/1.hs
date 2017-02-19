
{-1. Define a function repl :: String -> String that repeats each letter twice.
Examples:

repl "abcde" = "aabbccddee"
repl " " = "  "


2. Define a function remDup :: [Int] -> [Int] that removes duplicates from a list of integers. That is, if any element of the list is repeated many times, only the first occurrence of the element should be retained and the others discarded.

Examples:

remDup [1,1,2,2,3,3,4,4,4] = [1,2,3,4]
remDup [1,2,3,3,2] = [1,2,3]

3. Define a function remChamp :: [Int] -> [Int] that removes the first occurrence of the largest number in the input list and leaves the remaining list undisturbed. If the input list is empty, it should return the empty list.

Examples:

remChamp [22, 35, 4, 65] = [22,35,4]
remChamp [1,5,2,3,5,4] = [1,2,3,5,4]
remChamp [5] = []

4. Define a function remRunnerUp :: [Int] -> [Int] that removes the second largest number in the input list and leaves the remaining list undisturbed. The second largest number is defined to be the
second number in the list if the list is sorted in descending order.  So, in particular, if the largest number appears twice, the second largest number is the same as the largest number.  If
the input list is has less than two elements, the function should return the input list unchanged.

Examples:

remRunnerUp [22, 35, 4, 65] = [22,4,65]
remRunnerUp [1,5,2,3,5,4] = [1,2,3,5,4]
remRunnerUp [5] = [5]
-}


import Data.List

repl [] = []
repl (x:xs) = x:x:repl (xs)


remDup [] = []
remDup (x:xs) = x:remDup(mremove x xs) 
	where
	mremove x [] = []
	mremove x (y:ys)
		| x == y =    mremove x ys
		| otherwise = y:mremove x ys
	
lar x [] = x
lar x (y:ys) 
	| x>y = lar x ys
	| otherwise = lar y ys 
	
remChamp [] = []
remChamp [x] = []
remChamp (y:ys) 
	| (lar y ys) == y = ys
	| otherwise = y:remChamp ys


slar [x] = x	
slar l = (sort l)!!(length l -2)

remRunnerUp [] = []
remRunnerUp [x] = [x]
remRunnerUp (y:ys) 
	| (slar (y:ys)) == y = ys
	| otherwise = y:remRunnerUp ys


	



