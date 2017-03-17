import Data.Array

--fibonacci series using arrays

fin n  = fiba!n

fiba :: Array Int Integer
fiba   = listArray (0,100) [ f i | i <- [0..100] ]
	where 
		f 0 = 1
		f 1 = 1
		f i = fiba!(i-1) + fiba!(i-2)
