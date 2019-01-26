{-# LANGUAGE Haskell98 #-}

-- | __Alumno:__ Cazajous Miguel A.
--
-- __Asignatura:__ Paradigmas de programación
--
-- __Docente:__ Ing. Wolfmann Gustavo
--
-- __Tema:__
--
--      - Algoritmo minimax para elección de mejor movimiento en el juego TA TE TI
--
-- __Requisitos:__ 
-- 	
-- 	- Se necesita instalar ghc y el módulo random.
--
-- sudo pacman -S ghc ghc-static (Para distribuciones basadas en Arch)
--
-- El módulo debe ser descargado de <https://hackage.haskell.org/package/random> 
--
-- y se instala siguiendo las instrucciones de <https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package>
--
-- __Ejecución:__
--
-- 	- Para ejecutar el programa usar __make arg=file__ 
--
-- __Descripción:__
--
--      - El programa comienza tomando un archivo del cual lee un estado de juego, luego muestra el contenido
--
-- obtenido representado en un tablero y verifica que este cumpla con un estado válido, de no serlo el programa 
--
-- finaliza con un mensaje de error, de lo contrario continua y verifica si hay un ganador (de haberlo lo muestra
--
-- por pantalla), si no hay, sigue y comprueba que jugador debe continuar jugando, dándole prioridad al jugador
--
-- __O__ en caso de que exista la misma cantidad de __O__ que se __X__.
--
-- El jugador __X__ elige entre los posibles valores disponibles uno al azar haciendo uso del módulo __Random__.
--
-- Aunque el jugador __O__ sea un jugador perfecto, pues es el que implementa el algoritmo minimax, aún es posible
--
-- que este pierda si el siguiente turno es del jugador __X__ y la función random justo elige el valor
--
-- que lo consagra ganador, de otra forma el jugador __O__ será el ganador.
--
-- Para algunos estados de juego donde el jugador __O__ puede ganar con un simple movimiento se puede observar 
--
-- que esto no ocurre siempre y se debe a que cuando se consideran los puntajes solo se verifica que __X__ no 
--
-- sea el ganador, pero no se considera la profundidad, siempre elige el primero valor de la lista de puntajes,
--
-- sea o no la forma más rápida de consagrarse ganador. Por ese motivo puede demorarse más la victoria del 
--
-- jugador __O__.
module Main where

import System.Random

-- | Función principal. Lee un archivo con un estado del juego,
--
-- Invoca a la función __'unaLinea'__ y luego a la función __'juego'__
--
main :: IO ()
main = do
    --  Lee contenido de un archivo y lo guarda en cadena
    cadena <- getContents
    let tablero = unaLinea cadena
    --  Imprime el tablero
    putStrLn "\nParadigmas de programación\n--------------------------\nTA TE TI\n--------\n"
    putStrLn "\nEstado inicial\n--------------\n"
    juego tablero


-- | Omite los espacios y saltos de línea que se leyeron del archivo
--
-- Recibe un String (xs) y remueve cada caracter que sea igual a un salto de línea 
--
-- o un espacio. 
unaLinea :: String -> String
unaLinea xs = [x | x <- xs, x/='\n', x/=' ']


-- | Llama a __'mostrarString'__ para mostrar una representación del estado del juego y 
--
-- luego verifica mediante __'formato'__ que el estado recibido es correcto. Sino lo es sale del programa 
--
-- con un mensaje de error.
--
-- Verifica si hay un ganador llamando a la función __'ganador'__. Si el valor devuelto no es un caracter 
--
-- espacio, es decir que hay un ganador, lo muestra. En caso contrario continua.
--
-- Si no se ha encontrado un ganador define cual es el jugador que debe continuar llamando a __'proxJugador'__
--
-- Para el caso del jugador __X__ elige una posición aleatoria entre las que son válidas (__'movPermitidos'__)
--
-- y realiza el movimiento llamando a la función __'mover'__. Para el valor aleatorio tomamos la longitud total
--
-- de la lista devuelta por __'movPermitidos'__ menos 1 (la lista comienza desde 0). Finalmente vuelve a llamar 
--
-- la función __'juego'__ de manera recursiva para continuar con el juego. 
--
-- Si el próximo jugador es __O__ primero verifica si es el primer movimiento del juego, de serlo coloca __O__ 
--
-- en el centro del tablero, en caso contrario llama a la función __'mejorMovimiento'__.
--
-- Luego de realizado el movimiento se llama a __'juego'__ nuevamente para continuar con el proceso.
juego tablero = do
  putStr $ mostrarString tablero
  if not $ formato tablero
    then error "Formato de archivo inválido"
    else do
      if (ganador tablero) /= ' '
        then putStrLn $ "\nEl ganador es el jugador " ++ show (ganador tablero) ++ "\n"
        else do
          if 'X' == (proxJugador tablero)
            then do
              putStrLn "\nTurno jugador X\n"
              num <- randomRIO (0,length (movPermitidos tablero)-1) :: IO Int
              juego (mover tablero 'X' ((movPermitidos tablero)!!num))
            else do
              if (cantE tablero == 9)
                then do
                  putStrLn "\nTurno jugador O\n"
                  juego (mover tablero 'O' 4)
                else do
                  putStrLn "\nTurno jugador O\n"
                  juego (mover tablero 'O' (mejorMovimiento tablero))


-- | Muestra una representación del tablero considerando el estado que se leyó desde el archivo
mostrarString :: String -> String
mostrarString tablero =
  "+---+---+---+\n" ++
  "|   |   |   |\n" ++
  "| " ++ tablero !! 0 : [] ++ " | " ++ tablero !! 1 : [] ++ " | " ++ tablero !! 2 : []  ++ " |\n" ++
  "|   |   |   |\n" ++
  "+---+---+---+\n" ++
  "|   |   |   |\n" ++
  "| " ++ tablero !! 3 : [] ++ " | " ++ tablero !! 4 : [] ++ " | " ++ tablero !! 5 : []  ++ " |\n" ++
  "|   |   |   |\n" ++
  "+---+---+---+\n" ++
  "|   |   |   |\n" ++
  "| " ++ tablero !! 6 : [] ++ " | " ++ tablero !! 7 : [] ++ " | " ++ tablero !! 8 : []  ++ " |\n" ++
  "|   |   |   |\n" ++
  "+---+---+---+\n"


-- | Verifica que el formato del estado de juego obtenido del archivo sea válido
--
-- Esto implica que no contenga elementos diferentes a __X__, __O__ y __E__ y además que
--
-- la diferencia entre cantidad de __X__ y __O__ no sea mayor a 1
--
-- Utiliza las funciones __'cantX'__, __'cantO'__ y __'cantE'__
formato :: String -> Bool
formato tablero
  | abs(x-o)<=1 && x + o + e == 9 = True
  | otherwise = False
  where
  (x,o,e)=(cantX tablero, cantO tablero, cantE tablero)


-- | Cuenta la cantidad de __X__ que tiene el estado de juego.
--
-- Suma 1 cada vez que un caracter de la cadena es igual a __X__.
cantX :: String -> Int
cantX xs = sum [1 | x<-xs, x=='X']


-- | Cuenta la cantidad de __O__ que tiene el estado de juego.
--
-- Suma 1 cada vez que un caracter de la cadena es igual a __O__.
cantO :: String -> Int
cantO xs = sum [1 | x<-xs, x=='O']


-- | Cuenta la cantidad de __E__ que tiene el estado de juego.
--
-- Suma 1 cada vez que un caracter de la cadena es igual a __E__.
cantE :: String -> Int
cantE xs = sum [1 | x<-xs, x=='E']


-- | Verifica cada línea vertical, horizontal y diagonal en busca de un ganador.
-- 
-- Si lo encuentra devuelve el ganador (__X__ u __O__), sino devuelve un caracter vacío.
--
-- Primero verifica que el comienzo de cada fila no sea una __E__ (empty) y luego que los valores 
--
-- de esa línea sean iguales. Si lo son devuelve ese valor que puede ser __X__ o __O__.
--
-- De igual manera se procede con las columnas y con las diagonales.
ganador :: String -> Char
ganador t
  -- Líneas horizontales
  | (t !! 0) /= 'E' && ((t !! 0) == (t !! 1) && (t !! 0) == (t !! 2)) = t !! 0
  | (t !! 3) /= 'E' && ((t !! 3) == (t !! 4) && (t !! 3) == (t !! 5)) = t !! 3
  | (t !! 6) /= 'E' && ((t !! 6) == (t !! 7) && (t !! 6) == (t !! 8)) = t !! 6
  -- Líneas verticales
  | (t !! 0) /= 'E' && ((t !! 0) == (t !! 3) && (t !! 0) == (t !! 6)) = t !! 0
  | (t !! 1) /= 'E' && ((t !! 1) == (t !! 4) && (t !! 1) == (t !! 7)) = t !! 1
  | (t !! 2) /= 'E' && ((t !! 2) == (t !! 5) && (t !! 2) == (t !! 8)) = t !! 2
  -- Líneas diagonales
  | (t !! 0) /= 'E' && ((t !! 0) == (t !! 4) && (t !! 0) == (t !! 8)) = t !! 0
  | (t !! 2) /= 'E' && ((t !! 2) == (t !! 4) && (t !! 2) == (t !! 6)) = t !! 2
  -- No hay ganador
  | otherwise = ' '


-- | Define que jugador tiene el siguiente turno usando __'cantX'__ y __'cantO'__
--
-- El jugador elegido para ser el siguiente será el que tenga un movimiento menos.
--
-- Nótese que en caso de igualdad siempre el jugador __O__ mueve primero.
proxJugador :: String -> Char
proxJugador tablero
  | cantX tablero < cantO tablero = 'X'
  | otherwise = 'O'


-- | Arma una lista con todas posiciones vacías.
--
-- Utiliza la función __'ganador'__ y la función __'esValido'__
--
-- Recibe una cadena con un estado de juego, si existe un ganador devuelve una lista vacía
--
-- que representa que no hay movimientos válidos.
--
-- De lo contrario usando la función __'esValido'__ devuelve una lista de enteros en las 
--
-- posiciones donde todavía puede jugarse. 
movPermitidos :: String -> [Int]
movPermitidos tablero 
  | (ganador tablero) /= ' ' = []
  | otherwise = [y | y <- [0..8], (esValido tablero y)]


-- | Retorna verdadero si el movimiento es válido
--
-- Recibe un entero que verifica que esté entre 0 y 9, y un estado de juego.
--
-- Si el valor coincide donde se encuentra el caracter __E__ devuelve verdadero, de lo contrario 
--
-- devuelve falso.
esValido :: String -> Int -> Bool
esValido tablero p
  | p < 0 || p >= 9           = False   
  | tablero !! p == 'E'       = True    
  | otherwise                 = False   


-- | Esta función realiza el movimiento del jugador a la posición enviada y devuelve un nuevo tablero
--
-- actualizado donde se encuentra representado el último movimiento.
-- 
-- Recibe un estado del juego, __X__ y/o __O__ y una posición válida.
--
-- Del estado recibido separa el primer valor y si este es 0 (otherwise) se marca el primer lugar
--
-- con el caracter enviado (__X__,__O__).
--
-- Si el valor es mayor que 0 comienza una llamada recursiva. Empieza a llenar una lista vacía con el primer 
--
-- valor del String en cada llamada recursiva, al tener un valor menos la posición se decrementa en 1 hasta
--
-- llegar a 0 donde se coloca el caracter recibido. Al final se concatenan los caracteres individuales extraídos
--
-- en cada llamada recursiva, el caracter enviado y el resto del String.
mover :: String -> Char -> Int -> String
mover (p:tablero) jugador pos
  | pos > 0 = p:[] ++ (mover tablero jugador (pos - 1))
  | otherwise = jugador:[] ++ tablero


-- | Retorna una posición que asegura no se pierde. Recibe un estado de juego y devuelve un entero.
--
-- Utiliza la función __'puntajeMovimientos'__ que devuelve una lista de tuplas que contiene el movimiento y 
--
-- su puntaje. Luego invoca a __'maximoPuntaje'__ que devuelve el movimiento que posee mayor puntaje.
--
-- para obtener la posición deseada. La función __foldr__ toma el primer valor de mov_puntaje y aplica la 
--
-- función con el último elemento de la lista (tail mov_puntaje), luego aplica la función con el resultado 
--
-- de la anterior y el penúltimo valor de la lista. Y así sucesivamente.
mejorMovimiento :: String -> Int
mejorMovimiento tablero = movimiento
  where
  mov_puntaje = puntajeMovimientos tablero
  (movimiento, puntaje) = foldr maximoPuntaje (head mov_puntaje) (tail mov_puntaje)


-- | Retorna una lista de tuplas (movimiento, puntaje)
--
-- Invoca a __'movPermitidos'__ y a __'mayorPuntaje'__.
--
-- Con la función zip se agrupan en tuplas cada movimiento con su respectivo puntaje.
--
-- en "tableros" (lista) se guardan tantos tableros como movimientos posibles haya. Cada uno
--
-- representando uno de esos movimientos.
--
-- En puntajes (lista) se guardan los puntajes de los estados de juegos guardados en la lista "tableros" 
puntajeMovimientos :: String -> [(Int, Int)]
puntajeMovimientos tablero = zip (movPermitidos tablero) puntajes
  where
  tableros = map (mover tablero 'O') (movPermitidos tablero)
  puntajes = map mayorPuntaje tableros


-- | Retorna el mayor puntaje de movimiento para el tablero recibido.
--
-- Invoca a __'movPermitidos'__, __'puntajeJugador'__, __'mover'__ y __'menorPuntaje'__
--
-- Sino existen más movimientos por hacer se fija si el jugador __O__ es el ganador o no, y recibe un puntaje
--
-- de acuerdo al resultado.
--
-- Al igual que antes en "tableros" se guardan todos los posibles movimientos con una longitud igual a 
--
-- la cantidad de __E__ que haya.
--
-- En este punto las funciones __'mayorPuntaje'__ y '__menorPuntaje__' comienzan a llamarse estre sí
--
-- hasta cubrir con la totalidad de movimientos que quedan por delante hasta finalizar el juego,
--
-- intercalando entre los jugadores.
--
-- En __otherwise__ se ordenan los puntajes, cuando se juega __X__ se considera el menor puntaje, pues
--
-- es el que hace perder al jugador __O__, de lo contrario se considera el mayor. 
mayorPuntaje :: String -> Int
mayorPuntaje tablero
  | length (movPermitidos tablero) == 0    = puntajeJugador tablero 'O'
  | otherwise = foldr min (head puntajes) (tail puntajes)
  where
  tableros = map (mover tablero 'X') (movPermitidos tablero)
  puntajes = map menorPuntaje tableros


-- | Retorna el menor puntaje de movimiento para el tablero recibido
--
-- Invoca a __'movPermitidos'__, __'puntajeJugador'__, __'mover'__ y __'mayorPuntaje'__
-- 
-- Sino existen más movimientos por hacer se fija si el jugador 'O' es el ganador o no.
--
-- Al igual que antes en "tableros" se guardan todos los posibles movimientos con una longitud igual a 
--
-- la cantidad de __E__ que haya.
menorPuntaje :: String -> Int
menorPuntaje tablero
  | length (movPermitidos tablero) == 0    = puntajeJugador tablero 'O'
  | otherwise = foldr max (head puntajes) (tail puntajes)
  where
  tableros = map (mover tablero 'O') (movPermitidos tablero)
  puntajes = map mayorPuntaje tableros


-- | Asigna un puntaje a un jugador recibido.
--
-- Usando la función __'ganador'__, si no hay ganador devuelve 0, si el ganador es el jugador que se 
--
-- recibió (Char) devuelve 10, sino lo es devuelve -10.
--
-- Como se vió antes, siempre se comprueba usando al jugador __O__.
puntajeJugador :: String -> Char -> Int
puntajeJugador tablero jugador
  | (ganador tablero) == ' '     = 0
  | (ganador tablero) == jugador  = 10
  | otherwise                 = -10

-- | De dos tuplas (movimiento, puntaje) devuelve la de mayor puntaje.
maximoPuntaje (m0, s0) (m1, s1)
  | s0 > s1 = (m0, s0)
  | otherwise = (m1, s1)
