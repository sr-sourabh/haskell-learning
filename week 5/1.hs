import Data.List

{-


1. 	G H Hardy recounted the following anectode about Srinivasa Ramanujan..

	    I remember once going to see him when he was ill at Putney. I had ridden in taxi cab number 1729 and remarked that the number 
	    seemed to me rather a dull one, and that I hoped it was not an unfavorable omen. "No," he replied, "it is a very interesting 
	    number; it is the smallest number expressible as the sum of two cubes in two different ways."

	Define a Ramanujan number to be a positive integer that can be expressed as a sum of two cubes in at least two different ways. The 
	anecdote above says that 1729 is the smallest Ramanujan number: (1729 = 13 + 123 = 93 + 103).

	Define a function

	ramanujan :: Int -> Int

	such that

	ramanujan n

	is the nth smallest positive number that can be expressed  as a sum of two cubes in at least two different ways.

	Examples:

	ramanujan 1 = 1729
	ramanujan 4 = 20683




-}

-----------------------------solution 1 -----------------------------
ramanujan x = [  (a^3+d^3) | a<-[1..],b<-[1..a-1] , c<-[1..b-1] , d<-[1..c-1], (a^3 + d^3) == (b^3 + c^3)  ] !! (x-1) 




{-

2.  	A two-dimensional matrix can be represented as a list of rows, each row itself being a list of elements. So in general it is 
	of type 
	[[a]]. Not every list of lists is a matrix, though. For instance, [[1,2,3], [], [2,4]] is a list of three lists, each of a 
	different size.

	(a) Define a function

	is_matrix :: [[a]] -> Bool

	that checks if a list of lists is a valid matrix (nonzero number of rows, each of the same nonzero length).

	Examples:

	is_matrix [] = False
	is_matrix [[],[],[]] = False
	is_matrix [[2,3], [4,5], [6,7]] = True
	is_matrix [[2,3,4,5,6,7]] = True

	(b) A square matrix is one where the number of rows is equal to the number of columns. Define a function

	is_square_matrix :: [[a]] -> Bool

	that checks if a list of lists is a square matrix.

	Examples:

	is_square_matrix [] = False
	is_square_matrix [[],[],[]] = False
	is_square_matrix [[1]] = True
	is_square_matrix [[1,2,3], [4,5,6], [7,8,9]] = True

	(c) Given two integer matrices m1 and m2, they can be added if the number of rows and number of columns are the same. Define a 
	function

	addable :: [[Int]] -> [[Int]] -> Bool

	that checks if two matrices are addable.

	Examples:

	addable [[1,2],[3,4]] [[1,2,3],[4,5,6]] = False
	addable [[1,2],[3,4]] [[5,6],[7,8]] = True

	(d) Define a function that adds two matrices if they are addable (and returns the empty list if they are not addable).

	add_matrix :: [[Int]] -> [[Int]] -> [[Int]]

	Examples:

	add_matrix [[1,2,3,4]] [[5,6,7,8]] = [[6,8,10,12]]
	add_matrix [[1,2],[3,4]] [[5,6],[7,8]] = [[6,8],[10,12]]

	(e) Given two integer matrices m1 and m2, they can be multiplied if the number of columns in m1 is the same as the number of rows 
	in m2. 
	Define a function

	multiplyable :: [[Int]] -> [[Int]] -> Bool

	that checks if two matrices are multiplyable.

	Examples:

	multiplyable [[1,2,3],[4,5,6]] [[1,2],[3,4]] = False
	multiplyable [[1,2],[3,4]] [[1,2,3],[4,5,6]] = True

	(f) Define a function that mulitplies two matrices if they are multiplyable (and returns the empty list if they are not 
	multiplyable).

	multiply_matrix :: [[Int]] -> [[Int]] -> [[Int]]

	Examples:

	multiply_matrix [[1,2],[3,4]] [[1,2,3],[4,5,6]] = [[9,12,15],[19,26,33]]
	multiply_matrix [[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] = [[22,28],[49,64]]


-}




-----------------------------solution 2a-----------------------------
is_matrix [] = False
is_matrix [[]] = False
is_matrix [y] = True
is_matrix (x:xs)
	| x == [] = False
	|  length x == length (head xs) = is_matrix xs
	| otherwise = False  
	
	
	

-----------------------------solution 2b----------------------------	
is_square_matrix l = is_matrix l && ( length (head l) == length l )




-----------------------------solution 2c----------------------------
addable [] [] = False
addable l1 l2 = (length l1 == length l2) && sum (map length l1) == sum (map length l2)




-----------------------------solution 2d----------------------------
add_matrix [] [] = []
add_matrix (x:xs) (y:ys)
	|addable (x:xs) (y:ys)   = (zipWith (+) x y):add_matrix xs ys 
	|otherwise =  []
	
	
	
-----------------------------solution 2e-----------------------------

multiplyable l1 l2 
	|not ((is_matrix l1) && (is_matrix l2)) = False
	| length (head l1) == length l2 = True
    | otherwise = False



-----------------------------solution 2f-----------------------------
multiply_matrix l1 l2 
	| multiplyable l1 l2 = [[ sum ( zipWith (*) ar bc ) | bc <- (transpose l2)] | ar <- l1 ]
	| otherwise = []
	













	





 
