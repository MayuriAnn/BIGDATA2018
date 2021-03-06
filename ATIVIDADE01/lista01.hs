module Main where
import Data.Char
--exercicio 01
x = 2*3+5
y = 2+2*3+1
z = 3^4+5*2^5+1

---exercicio 02
mult3 x = (x `rem` 3)==0

---exercicio 03
mult5 x = (x `rem` 5)==0

---exercicio 04
mult35 x = (mult3 x) && (mult5 x)

---exercicio 05
checkCond x = (x<(-1)) || (x>1 && x `rem` 2==0)

---exercicio 06
div2d :: Integer -> Double
div2d x = fromInteger x / 2

--exercicio 07
sin2d x = (x1,x2)
    where
        x1 = sqrt (1-cos x)/2
        x2 = (-x1)

--exercicio 08
bissexto ano = ano `rem` 400 == 0 || ano `rem` 4 == 0 && ano `rem` 100 /= 0

anosBiss = [ano | ano <- [1..2018], bissexto ano]

--exercicio 9 
f10 = take 10 anosBiss
l10 = drop (length anosBiss - 10) anosBiss

--exercicio 10
fhalf = take (length anosBiss `div` 2) anosBiss
lhalf = drop (length anosBiss `div` 2) anosBiss
t = (fhalf, lhalf) 

--exercicio 11
myConcat :: [Char] -> [Char] -> [Char]
myConcat s1 s2 = s1 ++ (' ' : s2)

--exercicio 12

s="0123456789"
digitos = [digitToInt d | d <- s, isDigit d] --permite apenas numeros
--digitos = map digitToInt s


-- |'main' executa programa principal
main :: IO ()
main = do
    print ("exercicio 1: nao eh necessario parenteses")
    print ("mult3 6")
    print (mult3 6)
    print ("mult3 7")
    print (mult3 7)
    print ("mult5 20")
    print (mult5 20)
    print ("mult5 22")
    print (mult5 22)
    print ("mult35 15")
    print (mult35 15)
    print ("mult35 12")
    print (mult35 12)
    print ("checkCond (-2)")
    print (checkCond (-2))
    print ("checkCond 6")
    print (checkCond 6)
    print ("checkCond 5")
    print (checkCond 5)    
    print ("checkCond 0")
    print (checkCond 0)
    print ("div2d 6")
    print (div2d 6)
    print ("sin2d 30")
    print (sin2d 30)
    print ("ano bissexto ate atual")
    print (anosBiss) 
    print ("10 primeiros ano bissexto")
    print (f10) 
    print ("10 ultimos ano bissexto")
    print (l10) 
    print ("exercicio 10")
    print (t) 
    print ("concat 'foo' 'bar'")
    print (myConcat "foo" "bar")   
    print ("exercicio 12")
    print (digitos)
    