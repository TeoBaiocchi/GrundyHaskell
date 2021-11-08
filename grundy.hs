import Control.Exception

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

inputMaloInicio = do
    putStrLn "Input invalido"
    validarInputInicial

inputMaloFila xs = do
    putStrLn "Input Invalido"
    ingresarFila xs

inputMaloTupla (x:xs) fila = do
    putStrLn "Input Invalido"
    ingresarTupla (x:xs) fila

validarInputInicial :: IO Int
validarInputInicial = do
    input1 <- getLine
    n <- try (evaluate (read input1 :: Int))
    case n of
        Left (_ :: SomeException) -> inputMaloInicio
        Right val -> if val <= 2 then inputMaloInicio else (return val)

ingresarFila :: [Int] -> IO Int    
ingresarFila xs = do
    input1 <- getLine
    n <- try (evaluate (read input1 :: Int))
    case n of
        Left (_ :: SomeException) -> inputMaloFila xs
        Right val -> if val <= 0 || val > length(xs) || (xs !! (val-1))<=2 then inputMaloFila xs else (return val)
    
ingresarTupla :: [Int] -> Int -> IO (Int, Int)
ingresarTupla (x:xs) fila = do
    input1 <- getLine
    n <- try (stringTup input1)
    case n of
        Left (_ :: SomeException) -> inputMaloTupla (x:xs) fila
        Right val -> if (fst val) <= 0 || (snd val) <= 0 || ((fst val)+(snd val))/=((x:xs)!!fila) || (fst val) == (snd val) then inputMaloTupla (x:xs) fila else (return val)

-- stringTup :: String -> IO (Int, Int)
-- stringTup s = do
--     [(n, s')] <- return $ (reads s  :: [(Int, String)])
--     [(c, _)]  <- return $ (reads s' :: [(Int, String)])
--     return (n, c)

stringTup :: String -> IO (Int, Int)
stringTup (y:x:xs) = do
    [(n, a:s)]  <- return $ (reads (x:xs)  :: [(Int, String)])
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
