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


	



