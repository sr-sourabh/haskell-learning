import Control.Monad



intArray 0 = return []
intArray x = do
    str <- getLine
    nextInt <- intArray (x - 1)
    
    return (str:nextInt)

-- Main Function
main = do
    array <- intArray 6
    display array
    
display l = putStrLn (show l)


    
    
    
    

