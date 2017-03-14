data Avl a = Nil | Node (Avl a) a Int (Avl a) --Int is for storing height



--lets write a height function first

height Nil = 0
height (Node t1 x h t2) = h

--lets now write a function to check fr skewness of a tree (we use this to check if tree is balanced or not)

slope Nil = 0
slope (Node t1 x h t2) = height t1 - height t2


--now lets write a rotateright function

rotateright (Node (Node tll y hl tlr) x h tr) = Node tll y nh (Node tlr x nhr tr)
	where 
		nhr = 1 + max (height tlr) (height tr)
		nh = 1+ max (height tll ) nhr
		
		
--similiarly lets write leftrotate

rotateleft (Node tl x h (Node trl y hr trr)) = Node (Node tl x nhl trl) y nh trr
	where 
		nhl = 1 + max (height tl ) (height trl)
		nh = 1 + max (height trr) nhl
		
-- now let us think about most crucial function in all of avl :p how to rebalance?????


rebalance (Node tl x h tr) 
	| abs (st) < 2 = Node tl x h tr   -- no rebalance needed and tree is balanced (st is slope of the tree c below)
	
	
	| st == 2 && stl /= -1 = rotateright (Node tl x h tr)   --normal case 1 rotation needed
	| st == 2 && stl == -1 = rotateright (Node (rotateleft tl ) x h tr) -- 2 rotation are needed as the tree is unbalanced side by side
	
	{-
		x
	       / \
	      y   z
	      	 / \
	      	a   b
	      	 \
	      	  c
	
	-}
	| st == -2 && str /= 1 = rotateleft (Node tl x h tr)   -- normal case 1 rotation needed
	| st == -2 && str == 1 = rotatelrft (Node tl x h (rotateright tr))  -- 2 rotation are needed as the tree is unbalanced side by side
	
	
	where 
		st = slope (Node tl x h tr)
		stl = slope tl
		str = slope tr
		
		
-- search function is cakewalk 
search Nil v  = False
search (Node tl x h tr) v
	| x == v = True
	| v < x = search tl v 
	| otherwise = search tr v
	
	
-- now lets finish insert 

insert Nil v = Node Nil v 1 Nil
insert (Node tl x h tr) v 
	| x == v = Node tl x h tr --no duplicates plz
	| v < x = rebalance (Node ntl x nhl tr) 
	| otherwise = rebalance (Node tl x nhr ntr)
	
	where
		ntl = insert tl
		ntr = insert tr
		nhl = 1 + max (height ntl) (height tr)
		nhr = 1 + max (height ntr) (height tl)
		
		
--delete is remaining
















