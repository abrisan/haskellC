func :: Int -> Bool
func x = x >= 1

func2 :: [Int] -> [Int]
func2 xs = filter func xs