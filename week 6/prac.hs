data BST a = Nil | Node (BST a ) a (BST a )
	deriving (Eq,Ord,Show)
	
	
maxt (Node t1 x t2) = max x (max y z)
	where 
		y = if (t1 == Nil) then x
		    else maxt t1
		z = if (t2 == Nil) then x
		    else maxt t2
		    
		    
mint (Node t1 x t2) = min x (min y z)
	where 
		y = if (t1 == Nil) then x
		    else mint t1
		z = if (t2 == Nil) then x
		    else mint t2	
	
isbst Nil = True
isbst (Node t1 x t2 ) = (isbst t1) && (isbst t2) && (t1 == Nil || maxt t1 < x) && (t2 == Nil || mint t2 > x) 


search Nil v = False
search (Node t1 x t2) v 
	| x == v = True
	| x > v = search t1 v
	| otherwise = search t2 v
	
	
insert Nil v = Node Nil v Nil
insert (Node l x r) v 
	| x == v = Node l x r
	| x > v = Node (insert l v) x r
	| otherwise = Node l x (insert r v)
	
	
delete Nil v = Node Nil v Nil
delete (Node l x r) v 
	| x == v = Node l x r
	| x > v = Node (delete l v) x r
	| otherwise = Node l x (delete r v)
	
	
	
	
	
	
	
