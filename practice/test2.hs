import System.Exit
import Control.Monad




intArray  = do
	str <- getLine
	if (str == "EOF")
		then ( return ()   )
		else 
			(nextInt<- intArray)
	return (str:nextInt)

-- Main Function
main = do
    array <- intArray  
    putStrLn (show (array))
    

