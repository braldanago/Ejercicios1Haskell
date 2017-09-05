division::Int->Int->Int
division a b = if (b>a) then 0 else 1 + division (a-b) b

dectobin::Int->Int
dectobin 0 = 0
dectobin 1 = 1
dectobin a = (mod a 2) +  10*(dectobin (div a 2))

bintodec::Int->Int
bintodec 0 = 0
bintodec 1 = 1
bintodec a = (mod a 10) + 2*(bintodec (div a 10))

indiceImpar:: [Int]->[Int]
indiceImpar [] = []
indiceImpar (x:y:xs) = y: (indiceImpar(xs)) 

sumaDigitos:: Int->Int
sumaDigitos a = if (mod a 10)==a then a else (mod a 10) + sumaDigitos(div a 10) 

contadorDigitos::Int->Int
contadorDigitos a 
	| a < 10 = 1 
	| otherwise = 1 + contadorDigitos(div a 10)

invertir::Int->Int
invertir a = if (mod a 10)==a then a else (10^(contadorDigitos (div a 10)))*(mod a 10) + invertir (div a 10) 

palindromo::Int->Bool
palindromo a
	| a == invertir a = True
	| otherwise = False

