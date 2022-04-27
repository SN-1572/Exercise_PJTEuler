main :: IO ()

main = do
 line1 <- getLine
 let ex = read line1 :: Int
 
 let result = foldl (+) 0 (filter iseven (fibonacciseq_entry (ex-1)))
 
 putStrLn $ show(result)


iseven :: Int -> Bool
iseven a
 | a `mod` 2 == 0 = True
 | otherwise = False

fibonacciseq_entry :: Int -> [Int]
fibonacciseq_entry a = (1 : 1 : (fibonacciseq a 1 1))
fibonacciseq :: Int -> Int -> Int -> [Int]
fibonacciseq a b c
 | (b+c) >= a = []
 | otherwise = (b + c) : fibonacciseq a c (b + c)
