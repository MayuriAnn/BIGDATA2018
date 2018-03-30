import Data.Char
---exercicio 01
ehTriangulo a b c =  a+b>c && a+c>b && b+c>a

---exercicio 02
tipoTriangulo a b c 
    |a==b && b==c = "equilatero"
    |a==b || b==c || a==c && ehTriangulo a b c = "isosceles"
    |a/=b && b/=c && a/=c && ehTriangulo a b c = "escaleno"
    |otherwise = "nao eh triangulo"

---exercicio 03
multEtiope m n 
    |m == 1 = n 
    |m `mod` 2/= 0 = n + multEtiope (m `div` 2) (n * 2)
    |otherwise = multEtiope (m `div` 2) (n * 2)

multEtiopeCaudal m n = multEtiope' m n 0
    where
        multEtiope' 0 n r = r
        multEtiope' m n r 
            | m `mod` 2 /= 0 = multEtiope' (m `div` 2) (n * 2) (r+n)
            | otherwise = multEtiope' (m `div` 2) (n * 2) (r)

---exercicio 04            
ehPrimo n = prime' n 2 0
    where
        prime' 1 _ _ = False
        prime' 2 _ _ = True
        prime' 3 _ _ = True
        prime' n i d
            |n==i && d==0 = True
            |n/=i && d/=0 = False
            |n `mod` i /=0 = prime' n (i+1) (d)
            |otherwise = prime' n (i+1) (d+1)

---exercicio 05
sumDigits n = sum [digitToInt d | d <- show n] 

---exercicio 06
persAditiva n = pers' n 0
    where
        pers' n r
            |(length $ show n) == 1 = r
            |otherwise = pers' (sumDigits n) (r+1)

---exercicio 07a
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = fatorial' n 1
  where
    fatorial' 1 r = r
    fatorial' n r = fatorial' (n-1) (n*r)

coefBinomial :: Integer -> Integer -> Integer 
coefBinomial m n = fatorial m `div` (fatorial n * fatorial(m-n))

---exercicio 07b
coefBinRecursive :: Integer -> Integer -> Integer    
coefBinRecursive m n
    |n==0 = 1
    |m==n = 1
    |otherwise =  (coefBinRecursive (m-1) (n-1)) + (coefBinRecursive (m-1) (n))

---exercicio 08  
pascalTriangleIJ :: Integer -> Integer -> Integer 
pascalTriangleIJ i j = coefBinRecursive i j

-- |'main' executa programa principal
main :: IO ()
main = do
    print ("ehTriangulo 1 2 3")
    print (ehTriangulo 1 2 3)
    print ("ehTriangulo 5 5 5")
    print (ehTriangulo 5 5 5)
    print ("tipoTriangulo 5 5 5")
    print (tipoTriangulo 5 5 5)
    print ("tipoTriangulo 5 5 9")
    print (tipoTriangulo 5 5 9)
    print ("tipoTriangulo 6 4 9")
    print (tipoTriangulo 6 4 9)
    print ("tipoTriangulo 1 2 3")
    print (tipoTriangulo 1 2 3)
    print ("multEtiope 12 12")
    print (multEtiope 12 12)
    print ("multEtiopeCaudal 12 12")
    print (multEtiopeCaudal 12 12)
    print ("ehPrimo 7919")
    print (ehPrimo 7919)
    print ("sumDigits 123456789")
    print (sumDigits 123456789)
    print ("persAditiva 123456789")
    print (persAditiva 123456789)
    print ("coefBinomial 10 5")
    print (coefBinomial 10 5)   
    print ("coefBinRecursive 10 5")
    print (coefBinRecursive 10 5) 
    print ("pascalTriangleIJ 7 3") 
    print (pascalTriangleIJ 7 3)








