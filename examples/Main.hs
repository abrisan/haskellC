func :: Int -> Bool
func x = x > 2

func2 :: [Int] -> [Int]
func2 xs = filter func xs
