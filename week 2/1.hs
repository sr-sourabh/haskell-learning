f :: [Int] -> Int
f xs = g (0,xs)

g :: (Int ,[Int]) -> Int 
g (m,[]) = m
g (m,x:xs) = g (m+1,xs)
