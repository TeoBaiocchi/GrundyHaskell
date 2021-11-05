main :: IO ()
main = do
    putStrLn "Bienvenido al juego del Grundy\nIngrese la cantidad de asteriscos para la linea inicial (mayor a 2): "
    linea <- validarInputInicial
    let listaLineas = [linea]
        in mostrarVariasLineas listaLineas 1
        
    juego [linea] 1

validarInputInicial = do
    input1 <- getLine
    if (read input1 :: Int) <= 2 then validarInputInicial else (return ((read input1) :: Int))

ingresarFila :: [Int] -> IO Int    
ingresarFila xs = do
    input1 <- getLine
    if (read input1 :: Int) <= 0 || (read input1 :: Int) > length(xs) then ingresarFila xs else (return ((read input1) :: Int))
    
ingresarTupla :: [Int] -> IO (Int, Int)
ingresarTupla (x:xs) = do
    input1 <- getLine
    x <- stringTup input1
    if (fst x) <= 0 || (snd x) <= 0 then ingresarTupla xs else (return x)

stringTup :: String -> IO (Int, Int)
stringTup s = do
    [(n, s')] <- return $ reads s
    [(c, _)]  <- return $ reads s'
    return (n, c)

mostrarVariasLineas :: [Int] -> Int -> IO ()
mostrarVariasLineas (x:[]) cont = do 
    putStrLn $ (show cont) ++ " : " ++ (mostrarLinea x)
mostrarVariasLineas (x:xs) cont = do
    putStrLn ((show cont) ++ " : " ++ mostrarLinea x)
    mostrarVariasLineas xs (cont+1)

mostrarLinea :: Int -> String
mostrarLinea 0 = ""
mostrarLinea num =  "*" ++ mostrarLinea (num-1)

agregarAlFinal n (x:[]) = (fst n):(snd n):[]
agregarAlFinal n (x:xs) = agregarAlFinal n xs

hayGanador :: [Int] -> Bool
hayGanador (x:xs) = if x > 2 then False else hayGanador xs

imprimirGanador :: Int -> IO ()
imprimirGanador jugador = do
  putStrLn $ "Ganó el jugador " ++ (show jugador)

juego :: [Int] -> Int -> IO ()
juego (x:xs) jugador = do
    putStrLn $ "Jugador " ++ (show jugador) ++ "\nElija fila: "
    input1 <- ingresarFila (x:xs)
    putStrLn "Ingrese division: "
    dividir <- ingresarTupla (x:xs)
    let lista = agregarAlFinal dividir (x:xs)
    if (hayGanador lista == False) then (let jugador = (mod jugador 2)+1 in (juego lista jugador)) else imprimirGanador jugador -- creo que tendria que ser asi la recursion y comprobacion de victoria
    
  
