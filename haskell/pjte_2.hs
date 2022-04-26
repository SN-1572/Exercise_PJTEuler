main :: IO ()

main = do
 line1 <- getLine
 let ex = read line1 :: Int
 
 let result = foldl (+) 0 (filter iseven (fibonacciseq_entry (ex-1)))
 
 putStrLn $ show(result)


threefivelist_below :: Int -> [Int]

threefivelist_below  a
 | a == 0 = []
 | modu a 3 == 0 = a : threefivelist_below (a-1)
 | modu a 5 == 0 = a : threefivelist_below (a-1)
 | otherwise = threefivelist_below (a-1)


modu :: Int -> Int -> Int
modu l r
 | l - r == 0 = 0
 | l - r >= 0 = modu (l-r) r
 | otherwise = l


iseven :: Int -> Bool
iseven a
 | (modu a 2) == 0 = True
 | otherwise = False


fibonacciseq_entry :: Int -> [Int]
fibonacciseq_entry a = (1 : 1 : (fibonacciseq a 1 1))
fibonacciseq :: Int -> Int -> Int -> [Int]
fibonacciseq a b c
 | (b+c) >= a = []
 | otherwise = (b + c) : fibonacciseq a c (b + c)
