validarInputInicial = do
    putStrLn ""
    putStr "Ingrese la cantidad de asteriscos para la linea inicial (mayor a 2): " 
    input1 <- getLine
    if (read input1 :: Int) <= 2 then validarInputInicial else (return ((read input1) :: Int))

ingresarFila :: [Int] -> IO Int    
ingresarFila xs = do
    putStr "Elija fila: "
    input1 <- getLine
    if (read input1 :: Int) <= 0 || (read input1 :: Int) > length(xs) then ingresarFila xs else (return ((read input1) :: Int))
    
ingresarTupla :: [Int] -> IO (Int, Int)
ingresarTupla (x:xs) = do
    putStrLn ""
    putStr "Ingrese division: "
    input1 <- getLine
    let x = (read input1 :: (Int, Int))
        in if (fst x) <= 0 || (snd x) <= 0 then ingresarFila xs else (return ((read input1) :: (Int, Int)))

mostrarVariasLineas :: [Int] -> Int -> IO String
mostrarVariasLineas [] cont = do 
    return ""
mostrarVariasLineas (x:xs) cont = do
    putStrLn ((show cont) ++ " : " ++ (mostrarLinea x))
    mostrarVariasLineas xs (cont+1)


mostrarLinea 0 = ""
mostrarLinea num =  "*" ++ mostrarLinea (num-1)

-- agregarAlFinal

hayGanador :: [Int] -> Bool
hayGanador (x:xs) = if x > 2 then False else hayGanador xs

imprimirGanador :: Int -> IO String
imprimirGanador jugador = do
    putStrLn "Ganó el jugador " ++ jugador

juego :: [Int] -> Int -> IO String
juego (x:xs) jugador = do
    putStrLn "Jugador " ++ jugador
    input1 <- ingresarFila (x:xs)
    putStrLn (show input1)
    dividir <- ingresarTupla (x:xs)
    let lista = agregarAlFinal
    if (hayGanador lista == False) then (let jugador = (mod jugador 2)+1 in (juego lista jugador)) else imprimirGanador jugador -- creo que tendria que ser asi la recursion y comprobacion de victoria
    
    
main = do
    putStrLn "Bienvenido al juego del Grundy"
    linea <- validarInputInicial
    putStrLn (show linea)
    let listaLineas = [linea]
        in mostrarVariasLineas listaLineas 1
        
    juego [linea] 1
