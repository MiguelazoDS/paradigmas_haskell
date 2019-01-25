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
-- sudo pacman -S ghc
--
-- Para el módulo descargarlo de <https://hackage.haskell.org/package/random> 
--
-- y seguir las instrucciones de <https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package>
--
-- __Descripción:__
--
--      - El programa comienza tomando un archivo del cual lee un estado de juego
--
-- muestra el contenido obtenido representado en un tablero y verifica que este cumpla con un estado válido,
--
-- de no serlo el programa finaliza, de ser válido continua y verifica si hay un ganador (de haberlo lo muestra por pantalla)
--
-- (es posible que el estado sea de un juego finalizado) si no hay sigue y comprueba que jugador debe continuar
--
-- (el que tiene un movimiento menos) el jugador en caso de que tengan la misma cantidad de movimientos es el jugador O
--
-- el cual es el jugador "imposible".
--
-- El jugador X elige entre los posibles valores disponibles uno al azar.
--
-- De acuerdo a como está implementado es posible que el jugador O pierda si el siguiente turno es del jugador X
--
-- y la función random justo elige el valor que lo consagra ganador, de otra forma el jugador O que es el que ejecuta el algoritmo
--
-- Minimax será el ganador.
--
-- Para algunos estados de juego donde el jugador O puede ganar con un simple movimiento se puede observar que esto no ocurre siempre
--
-- y se debe a que cuando se consideran los puntajes solo se verifica que X no sea el ganador de otra forma le asigna el mismo puntaje
--
-- a un movimiento que lo consagre ganador instantáneamente y a otro que no y luego se elige el primero de la lista.
--
-- Por ese motivo puede demorarse más de la lógica la victoria del jugador O
--
-- Para ejecutar el programa se hizo uso de las tuberías.
--
-- __cat archivo | runhaskell programa.hs__
--
-- Donde "archivo" representa a los diferentes estados del juego 

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
-- luego verifica mediante __'formato'__ que el estado recibido es correcto.
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
-- de la lista devuelta por __'movPermitidos'__ menos 1 (la lista comienza desde 0) 
--
-- En el caso del jugador 'O' la posición se determina llamando a __'mejorMovimiento'__
--
-- Luego de realizado el movimiento se __'juego'__ se llama a si misma con el tablero ya modificado con el último movimiento
--
-- Para el caso en que el tablero esté vacío para acelerar ese primer movimiento simplemente se mueve el jugador 'O'
--
-- (Ya que en caso de que esté vacío es el jugador por defecto) a la posición 4 del tablero (el centro)
juego :: String -> IO()
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


-- | Muestra una representación del tablero considerando el estado que se leyó desde archivo
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


-- | Cuenta la cantidad de 'X' que tiene el estado de juego
cantX :: String -> Int
cantX xs = sum [1 | x<-xs, x=='X']


-- | Cuenta la cantidad de 'O' que tiene el estado de juego
cantO :: String -> Int
cantO xs = sum [1 | x<-xs, x=='O']


-- | Cuenta la cantidad de 'E' que tiene el estado de juego
cantE :: String -> Int
cantE xs = sum [1 | x<-xs, x=='E']


-- | Verifica cada línea vertical, horizontal y diagonal en busca de un ganador.
-- 
-- Si lo encuentra devuelve el ganador (X u O), sino devuelve caracter vacío.
--
-- Primero verifica que el comienzo de cada fila no sea una E (empty) y luego que los valores 
--
-- de esa línea sean iguales. Si lo son devuelve ese valor que puede ser X o O.
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
-- definiendo como siguiente jugador el que tenga un movimiento menos
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
-- Si el valor coincide donde se encuentra el caracter "E" devuelve verdadero, de lo contrario 
--
-- devuelve falso.
esValido :: String -> Int -> Bool
esValido tablero p
  | p < 0 || p >= 9           = False   -- out of range
  | tablero !! p == 'E'       = True    -- empty
  | otherwise                 = False   -- played


-- | Esta función realiza el movimiento del Char a la posición Int y devuelve un nuevo tablero.
--
--
mover :: String -> Char -> Int -> String
mover (p:tablero) jugador pos
  | pos > 0 = p:[] ++ (mover tablero jugador  (pos - 1))
  | otherwise = jugador:[] ++ tablero


-- | Dado un tablero retorna una posición que asegura no se pierde
--
-- Utiliza la función __'puntajeMovimientos'__ y luego invoca a __'maximoPuntaje'__
--
-- para obtener la posición deseada
mejorMovimiento :: String -> Int
mejorMovimiento tablero = movimiento
  where
  mov_puntaje = puntajeMovimientos tablero
  (movimiento, puntaje) = foldr maximoPuntaje (head mov_puntaje) (tail mov_puntaje)


-- | Retorna una lista de tuplas (movimiento, puntaje)
--
-- Invoca a __'movPermitidos'__ y a __'mayorPuntaje'__
puntajeMovimientos :: String -> [(Int, Int)]
puntajeMovimientos tablero = zip (movPermitidos tablero) puntajes
  where
  tableros = map (mover tablero 'O') (movPermitidos tablero)
  puntajes = map mayorPuntaje tableros


-- | Retorna el mayor puntaje de movimiento para el tablero recibido
--
-- Invoca a __'movPermitidos'__, __'puntajeString'__, __'mover'__ y __'menorPuntaje'__
mayorPuntaje :: String -> Int
mayorPuntaje tablero
  | length (movPermitidos tablero) == 0    = puntajeString tablero 'O'
  | otherwise = foldr min (head puntajes) (tail puntajes)
  where
  tableros = map (mover tablero 'X') (movPermitidos tablero)
  puntajes = map menorPuntaje tableros


-- | Retorna el menor puntaje de movimiento para el tablero recibido
--
-- Invoca a __'movPermitidos'__, __'puntajeString'__, __'mover'__ y __'mayorPuntaje'__
menorPuntaje :: String -> Int
menorPuntaje tablero
  | length (movPermitidos tablero) == 0    = puntajeString tablero 'O'
  | otherwise = foldr max (head puntajes) (tail puntajes)
  where
  tableros = map (mover tablero 'O') (movPermitidos tablero)
  puntajes = map mayorPuntaje tableros


-- | Asigna un puntaje a un jugador recibido (Char).
puntajeString :: String -> Char -> Int
puntajeString tablero jugador
  | (ganador tablero) == ' '     = 0
  | (ganador tablero) == jugador  = 10
  | otherwise                 = -10


-- | De dos tuplas (movimiento, puntaje) devuelve la de mayor puntaje
maximoPuntaje :: (Int, Int) -> (Int, Int) -> (Int, Int)
maximoPuntaje (m0, s0) (m1, s1)
  | s0 > s1 = (m0, s0)
  | otherwise = (m1, s1)
