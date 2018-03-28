divisivel20 x = div20' x 1
    where
        div20' x r
            |r==21 = True
            |x `mod` r /=0 = False
            |otherwise = div20' x (r+1)

--projectEuler5 = proEu' 1
--    where
--        proEu' x
--            |divisivel20 x = x
--            |otherwise = proEu' (x+1)


projectEuler5 = lcm' 12 (lcm 11 12)
    where
        lcm' a b 
            |a>20 = b
            |otherwise = lcm' (a+1) (lcm a b)
            
fibonacci = 0 : 1 : prox fibonacci
    where
        prox (x:t@(y:resto)) = (x+y) : prox t

menor x = x<4000000

projectEuler2 = sum $ filter even $ takeWhile menor fibonacci

main :: IO ()
main = do
    print ("divisivel20 232792560")
    print (divisivel20 232792560) 
    print ("divisivel20 232792559")
    print (divisivel20 232792559) 
    print ("projectEuler5")
    print (projectEuler5)
    print ("take 9 fibonacci")
    print (take 9 fibonacci)
    print ("projectEuler2")
    print (projectEuler2)
    
