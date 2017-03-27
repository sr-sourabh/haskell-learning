deleteAllInstances :: Eq a => a -> [a] -> [a]
deleteAllInstances a (x:xs)
    | a == x    = rest
    | otherwise = x : rest
      where
        rest = deleteAllInstances a xs
deleteAllInstances _ _ = []






sumofsquares n = list
	
	where
		list =  [(i*i + j*j)| i<- [1..n] , j <- [1..n] , n == i*i + j*j ]
