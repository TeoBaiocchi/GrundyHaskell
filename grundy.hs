validarInputInicial = do
    putStrLn ""
    putStr "Inserte la cantidad de asteriscos para la linea inicial (mayor a 2): " 
    input1 <- getLine
    if (read input1 :: Int) <= 2 then validarInputInicial else (return ((read input1) :: Int))
    
-- validarInput x = 

mostrarVariasLineas :: [Int] -> Int -> IO String
mostrarVariasLineas [] cont = do 
    return ""
mostrarVariasLineas (x:xs) cont = do
    putStrLn ((show cont) ++ " : " ++ (mostrarLinea x))
    mostrarVariasLineas xs (cont+1)
    
mostrarLinea 0 = ""
mostrarLinea num =  "*" ++ mostrarLinea (num-1)

main = do
    putStrLn "Bienvenido al juego del Grundy"
    linea <- validarInputInicial
    putStrLn (show linea)
    let listaLineas = [linea]
        in mostrarVariasLineas listaLineas 1
