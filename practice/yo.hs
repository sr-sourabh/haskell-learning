import System.Exit

main  = first
	


first  = do
	n <- getLine
	if n == "EOF"	then exitSuccess
	else
		second
		
second  = do
	n <- getLine
	if n == "EOF"	then exitSuccess
	else
		third
		
third  = do
	n <- getLine
	if n == "EOF"	then exitSuccess
	else
		putStrLn (n)
	main
