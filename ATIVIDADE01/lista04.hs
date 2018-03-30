diag n = [[ if i==j then 1 else 0 | i <- [0..n-1]] | j <- [0..n-1]]

tamanhoMtx mtx = (m, n)
  where
    m = (length mtx)
    n = (length $ head mtx)

sumDiagPrinc m1 = sum [ m1!!i!!j | i <- [0..m-1], j <- [0..n-1],i==j ]
  where
    m = fst $ tamanhoMtx m1
    n = snd $ tamanhoMtx m1

sumDiagSecun m1 = sum [ m1!!i!!(n-i) | i <- [0..n]]
  where
    n = (-) (fst $ tamanhoMtx m1) 1
    m = (-) (snd $ tamanhoMtx m1) 1
 
main :: IO ()
main = do
print ("diag 5")
print (diag 5)
let d = [[5,2,7],[7,2,1],[6,6,6]]
print ("sumDiagPrinc d")
print (sumDiagPrinc d)
print ("sumDiagSecun d")
print (sumDiagSecun d)
