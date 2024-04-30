votosEnBlanco :: [(String,String)] -> [Float] -> Float -> Float
votosEnBlanco _ [] x = x
votosEnBlanco _ xs x = x - cantVotos xs

cantVotos :: [Float] -> Float 
cantVotos [] = 0
cantVotos (x:xs) = x + cantVotos xs  

formulasValidas :: [(String,String)] -> Bool 
formulasValidas [] = True
formulasValidas [(x,y)] | x == y = False
    | otherwise = True
formulasValidas (x:y:xs)  | fst x ==  fst y = False
    | snd x == snd y = False
    | fst x == snd y = False
    | snd x == fst y = False 
 | otherwise = pertenece x xs && pertenece y xs && formulasValidas xs 

pertenece :: (String,String) -> [(String,String)] -> Bool
pertenece x [] = True 
pertenece x (y:ys) | fst x ==  fst y = False
    | snd x == snd y = False 
    | fst x == snd y = False
    | snd x == fst y = False
    | otherwise = pertenece x ys 

porcentajeDeVotos :: String -> [(String,String)] -> [Float] -> Float  
porcentajeDeVotos x [] [] = 0
porcentajeDeVotos x (y:xs) t = ((iesimoElemento (lugarPresidente x (y:xs)) t) / cantVotos t ) * 100

lugarPresidente :: String -> [(String,String)] -> Float
lugarPresidente x [] = 0
lugarPresidente x (y:xs)  | x == fst y = 1
    |otherwise = 1 + lugarPresidente x xs 

iesimoElemento :: Float -> [Float] -> Float 
iesimoElemento _ [] = 0
iesimoElemento 1 (x:xs) = x
iesimoElemento n (x:xs) = iesimoElemento(n-1) xs 

proximoPresidente :: [(String,String)] -> [Float] -> String
proximoPresidente [t] [x] = fst t
proximoPresidente xs ys = aux2 (aux1 xs ys)

aux1 :: [(String,String)] -> [Float] -> [(String,String,Float)] 
aux1 [] [] = []
aux1 ((x,y):xs) (a:ys) = (x,y,a) : aux1 xs ys

aux2 :: [(String,String,Float)] -> String
aux2 [(a,b,c)] = a
aux2 ((a,b,c):(d,e,f):xs) | c > f = aux2((a,b,c):xs)
    |otherwise= aux2((d,e,f):xs)
