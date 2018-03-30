import Data.List
import Data.Ord
import Control.Parallel.Strategies

--exercicio 01
divisivel20 x = div20' x 1
    where
        div20' x r
            |r==21 = True
            |x `mod` r /=0 = False
            |otherwise = div20' x (r+1)

---exercicio 02
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

---exercicio 03            
fibonacci = 0 : 1 : prox fibonacci
    where
        prox (x:t@(y:resto)) = (x+y) : prox t

---exercicio 04
menor x = x<4000000
projectEuler2 = sum $ filter even $ takeWhile menor fibonacci

---exercicio 05
type Vector a = [a]
prodInterno :: Num a => Vector a -> Vector a -> a
prodInterno u v = sum $ zipWith (*) u v

---exercicio 06
collatz x
    |even x = x `div` 2
    |odd  x = 3*x + 1

---exercicio 07
collatzLen x = (+) 1 $ length $ takeWhile diff1 l
    where
        diff1 x = x /= 1 
        l = (x) : prox l 
            where
               prox (y:resto) = (collatz y) : prox resto

---exercicio 08 
              
collatzT x = (x, collatzLen x)
projectEuler14 = fst $ maximumBy (comparing snd) (map collatzT  [1..1000000] `using` parList rseq)

        
main :: IO ()
main = do
    print ("divisivel20 232792560")
    print (divisivel20 232792560) 
    print ("divisivel20 232792559")
    print (divisivel20 232792559) 
    print ("projectEuler5 - Pseudocodigo melhorado encontrado na internet")
    print (projectEuler5)
    print ("take 9 fibonacci")
    print (take 9 fibonacci)
    print ("projectEuler2")
    print (projectEuler2)
    print ("prodInterno ([1,2,3,4]) ([4,3,2,1])")
    print (prodInterno ([1,2,3,4]) ([4,3,2,1]))
    print ("collatz 2")
    print (collatz 4)
    print ("collatz 3")
    print (collatz 3)
    print ("collatzLen 9")
    print (collatzLen 9)
    print ("projectEuler14")
    print (projectEuler14)
    
