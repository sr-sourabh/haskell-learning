h :: [Int] -> [Bool]

h [] = []
h [x] = []
h (x:y:ys) = (x <= y): (h (y:ys)) 
