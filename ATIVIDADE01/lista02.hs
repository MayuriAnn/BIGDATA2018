ehTriangulo a b c =  a+b>c && a+c>b && b+c>a

tipoTriangulo a b c 
    |a==b && b==c = "equilatero"
    |a==b || b==c || a==c && ehTriangulo a b c = "isosceles"
    |a/=b && b/=c && a/=c && ehTriangulo a b c = "escaleno"
    |otherwise = "nao eh triangulo"


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
        
-- |'main' executa programa principal
main :: IO ()
main = do
    print (ehTriangulo 1 2 3)
    print (ehTriangulo 5 5 5)
    print (tipoTriangulo 5 5 5)
    print (tipoTriangulo 5 5 9)
    print (tipoTriangulo 6 4 9)
    print (tipoTriangulo 1 2 3)
    print (multEtiope 12 12)
    print (multEtiopeCaudal 12 12)
    print ("ehPrimo 7919")
    print (ehPrimo 7919)