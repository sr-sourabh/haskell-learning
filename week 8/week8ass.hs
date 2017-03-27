main = func []
    where
        f xs = unlines
                $ xs
        func xs = do
            line <- getLine
            if line == "EOF" then putStr (f xs) else func ( (reverse (line)):xs)
		
