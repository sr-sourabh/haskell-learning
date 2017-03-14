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
