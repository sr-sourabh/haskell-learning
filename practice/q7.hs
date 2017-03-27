main = func []
    where
        f xs = unlines
                . (\(l1, l2) -> l2 ++ l1)
                . splitAt (length xs `div` 2)
                . reverse
                $ xs
        func xs = do
            line <- getLine
            if line == "EOF" then putStr (f xs) else func (line:xs)
