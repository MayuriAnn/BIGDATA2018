divisivel20 x = div20' x 1
    where
        div20' x r
            |r==21 = True
            |x `mod` r /=0 = False
            |otherwise = div20' x (r+1)

main :: IO ()
main = do
    print ("divisivel20 232792560")
    print (divisivel20 232792560) 
    print ("divisivel20 232792559")
    print (divisivel20 232792559) 
    
