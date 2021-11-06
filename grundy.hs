main :: IO ()
main = do
    putStrLn "Bienvenido al juego del Grundy\nIngrese la cantidad de asteriscos para la linea inicial (mayor a 2): "
    linea <- validarInputInicial
    juego [linea] 1
    putStrLn "Presione enter para cerrar el programa"
    getLine
    putStr " "

juego :: [Int] -> Int -> IO ()
juego (x:xs) jugador = do
    mostrarVariasLineas (x:xs) 1
    putStrLn $ "Jugador " ++ (show jugador) ++ "\nElija fila: "
    input1 <- ingresarFila (x:xs)
    putStrLn "Ingrese division: "
    dividir <- ingresarTupla (x:xs) (input1-1)
    let lista = agregarSegunFila dividir (x:xs) (input1-1)
    if (hayGanador lista == False) then (let player = (mod jugador 2)+1 in (juego lista player)) else imprimirGanador jugador -- creo que tendria que ser asi la recursion y comprobacion de victoria

validarInputInicial :: IO Int
validarInputInicial = do
    input1 <- getLine
    if (read input1 :: Int) <= 2 then validarInputInicial else (return ((read input1) :: Int))

ingresarFila :: [Int] -> IO Int    
ingresarFila xs = do
    input1 <- getLine
    if (read input1 :: Int) <= 0 || (read input1 :: Int) > length(xs) || (xs !! ((read input1 :: Int)-1))<=2 then ingresarFila xs else (return ((read input1) :: Int))
    
ingresarTupla :: [Int] -> Int -> IO (Int, Int)
ingresarTupla (x:xs) fila = do
    input1 <- getLine
    n <- stringTup input1
    if (fst n) <= 0 || (snd n) <= 0 || ((fst n)+(snd n))/=((x:xs)!!fila) || (fst n) == (snd n) then ingresarTupla (x:xs) fila else (return n)

-- stringTup :: String -> IO (Int, Int)
-- stringTup s = do
--     [(n, s')] <- return $ (reads s  :: [(Int, String)])
--     [(c, _)]  <- return $ (reads s' :: [(Int, String)])
--     return (n, c)

stringTup :: String -> IO (Int, Int)
stringTup (y:x:xs) = do
    [(n, a:b:s)]  <- return $ (reads (x:xs)  :: [(Int, String)])
    [(c, s')] <- return $ (reads s :: [(Int, String)])
    return (n, c)


mostrarVariasLineas :: [Int] -> Int -> IO ()
mostrarVariasLineas (x:[]) cont = do 
    putStrLn $ (show cont) ++ " : " ++ (mostrarLinea x)
mostrarVariasLineas (x:xs) cont = do
    putStrLn $ (show cont) ++ " : " ++ (mostrarLinea x)
    mostrarVariasLineas xs (cont+1)

mostrarLinea :: Int -> String
mostrarLinea 0 = ""
mostrarLinea num =  "*" ++ mostrarLinea (num-1)

agregarSegunFila :: (Int, Int) -> [Int] -> Int -> [Int]
agregarSegunFila n (x:xs) 0    = (fst n):(snd n):xs
agregarSegunFila n (x:xs) fila = x:agregarSegunFila n xs (fila-1)
 
hayGanador :: [Int] -> Bool
hayGanador (x:[]) = if x > 2 then False else True
hayGanador (x:xs) = if x > 2 then False else hayGanador xs

imprimirGanador :: Int -> IO ()
imprimirGanador jugador = do
  putStrLn $ "Ganó el jugador " ++ (show jugador)
