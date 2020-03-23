-----------------------------------------------------------------------
-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1:Franco Colombini 41262669
-- INTEGRANTE 2:Santiago Silva 210/18
-- INTEGRANTE 3:Fabian Maximiliano Ko 343/18
-----------------------------------------------------------------------

data Desplazamiento = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

type Conjunto a = [a]
type Camino = [Desplazamiento]
type Posicion = (Integer,Integer)
type Tablero a = [[a]]
type CampoMinado = Tablero Bool
type TableroAF = Tablero Desplazamiento

-- Devuelve el tamaño de un tablero.
tamano :: Tablero a -> Integer
tamano t = fromIntegral(length t)

-- Devuelve el valor de una posición de un tablero.
-- Notar que la primera posición de arriba a la izquierda es la (1,1).
valor :: Tablero a -> Posicion -> a
valor t (i,j) = iesimo (iesimo t i) j
 
-- Devuelve el iésimo elemento de una lista. El primer elemento es el 1.
iesimo :: [a] -> Integer -> a
iesimo (x:xs) 1 = x
iesimo (x:xs) n = iesimo xs (n-1)

-- Determina si una posición está dentro de los límites de un tablero.
posValida :: Tablero a -> Posicion -> Bool
posValida t (i,j) = 1<=i && i<=n && 1<=j && j<=n
    where n = tamano t
    
    
-- Funciones de ejemplo, solo para ilustrar cómo usar los tipos definidos arriba.
-- Determina si un desplazamiento es vertical (Arriba o Abajo).
esVertical :: Desplazamiento -> Bool
esVertical Arriba = True
esVertical Abajo = True 
esVertical _ = False

-- Cuenta la cantidad de Desplazamientos verticales en un Camino.
contarDesplazamientosVerticales :: Camino -> Integer
contarDesplazamientosVerticales [] = 0
contarDesplazamientosVerticales (x:xs) | esVertical x = 1 + resto
                                       | otherwise    = resto
  where resto = contarDesplazamientosVerticales xs

-- Caminos de prueba.
camino1 = [Derecha, Abajo, Izquierda, Arriba, Abajo, Abajo, Derecha, Derecha]
camino2 = [Derecha, Abajo, Derecha, Abajo]
camino3 = [Derecha, Abajo, Derecha, Izquierda, Derecha, Abajo]

-- CampoMinado de prueba.
campo1 :: CampoMinado
campo1 = [ [False, False, True],
           [True,  False, False],
           [True,  True,  False] ]

-- TableroAF de prueba, sin ciclos.
taf1 :: TableroAF
taf1 = [ [Derecha,  Derecha, Abajo],
         [Arriba, Izquierda, Abajo],
         [Arriba, Izquierda, Abajo] ]

-- TableroAF de prueba, con ciclos.
taf2 :: TableroAF
taf2 = [ [Derecha,       Abajo, Abajo],
         [Arriba,    Izquierda, Abajo],
         [Izquierda, Izquierda, Izquierda] ]


--PARTE A. Campos Minados.


-- Devuelve la posicion luego de un desplazamiento
moverse :: Posicion -> Desplazamiento -> Posicion
moverse (x,y) Arriba = (x-1, y)
moverse (x,y) Abajo = (x+1, y)
moverse (x,y) Derecha = (x, y+1)
moverse (x,y) Izquierda = (x, y-1)

-- Muestra la lista de posiciones por las que paso el RAE dado un camino, cuando comienza por la posicion (1,1)
listaDeMovimientos :: Camino -> [Posicion]
listaDeMovimientos c = listaDeMovimientosDesdeY c (1,1)

-- Muestra la lista de posiciones por las que paso el RAE dado un camino, cuando comienza desde una posicion y
listaDeMovimientosDesdeY :: Camino -> Posicion -> [Posicion]
listaDeMovimientosDesdeY c y | length c == 0 = []
           |  otherwise = (moverse y (head c)) : listaDeMovimientosDesdeY (tail c) (moverse y (head c))

-- Determina si un camino se mantiene dentro de los limites del tablero a lo largo de su trayectoria, cuando comienza por la posicion (1,1)
caminoValido :: Tablero a -> Camino -> Bool
caminoValido t c = caminoValidoDesdeY t c (1,1) 

-- Determina si un camino se mantiene dentro de los limites del tablero a lo largo de su trayectoria, cuando comienza desde una posicion y
caminoValidoDesdeY :: Tablero a -> Camino -> Posicion -> Bool
caminoValidoDesdeY t c y | length c == 0 = True
            | posValida t (head (listaDeMovimientosDesdeY c y)) == False = False
            | otherwise = caminoValidoDesdeY t (tail c) (moverse y (head c))
            
-- Indica la ultima posicion dada una lista de posiciones
posFinal :: [Posicion] -> Posicion
posFinal (x:xs) | length (x:xs) == 1 = x
                | otherwise = posFinal xs

-- Determina si un RAE, comenzando en la posicion (1,1), al seguir el camino dado llega a la posicion (n,n) sin pisar ninguna mina
caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida s c | hayMina s (1,1) || caminoValido s c == False || posFinal (listaDeMovimientos c) /= (tamano s, tamano s) = False
                   | otherwise = caminoDeSalidaDesdeY s c (1,1)

-- Determina si un RAE, comenzando desde una posicion y, al seguir el camino dado llega a la posicion (n,n) sin pisar ninguna mina
caminoDeSalidaDesdeY :: CampoMinado -> Camino -> Posicion -> Bool
caminoDeSalidaDesdeY s c y | length c == 0 = True
                           | hayMina s (head (listaDeMovimientosDesdeY c y)) = False
                           | otherwise = caminoDeSalidaDesdeY s (tail c) (moverse y (head c))

-- Determina si hay una mina en una posicion dado un campo minado
hayMina :: CampoMinado -> Posicion -> Bool
hayMina s (a,b) = columnaB b (filaA a s)

-- Dada una fila toma el elemento en cierta columna (solo para CampoMinado)
columnaB :: Integer -> [Bool] -> Bool
columnaB n k | n == 1 = head k
             | otherwise = columnaB (n-1) (tail k)

-- Dado un CampoMinado toma una cierta fila
filaA :: Integer -> [[Bool]] -> [Bool]
filaA n k | n == 1 = head k
          | otherwise = filaA (n-1) (tail k)

-- Dado una lista de posiciones verifica si hay posiciones repetidas 
contieneRepe :: [Posicion] -> Bool
contieneRepe s | length s == 1 = False
               | elem (head s) (tail s) = True
               | otherwise = contieneRepe (tail s)

caminoDeSalidaSinRepetidos :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRepetidos s c = caminoDeSalida s c && (contieneRepe (listaDeMovimientos c) == False)
               
-- Dados un campo minado y un número natural k, devuelve el conjunto de todos los caminos de longitud k que lleven a un RAE desde (1, 1) hasta (n, n), sin pisar ninguna mina
salidasEnKDesp :: CampoMinado -> Integer -> Conjunto Camino
salidasEnKDesp cs k = sonCaminosDeSalida cs (caminosEnKDesp k)

-- Agrega un desplazamiento al final de todos los caminos en una lista
agregarATodas :: Desplazamiento -> [Camino] -> [Camino]
agregarATodas n ls | length ls == 0 = []
                   | otherwise =((head ls) ++ [n]) : (agregarATodas n (tail ls))

-- Agrega todos los desplazamientos en un conjunto al final de todos los caminos en una lista (probablemente se pueda explicar mejor)
agregarATodasLista :: Conjunto Desplazamiento -> [Camino] -> [Camino]
agregarATodasLista ls xs | length ls == 0 = []
               | otherwise = agregarATodas (head ls) xs ++ agregarATodasLista (tail ls) xs

-- Genera una lista de todos los caminos en existencia que contengan k desplazamientos
caminosEnKDesp :: Integer -> Conjunto Camino
caminosEnKDesp k | k == 0 = [[]]
                 | otherwise = agregarATodasLista [Derecha, Izquierda, Arriba, Abajo] (caminosEnKDesp (k - 1))

-- Dado un CampoMinado y un conjunto de Caminos devuelve el mismo conjunto sin los Caminos que no son caminos de salida para dicho campo
sonCaminosDeSalida :: CampoMinado -> Conjunto Camino -> Conjunto Camino
sonCaminosDeSalida cs xs | length xs == 0 = []
                         | caminoDeSalida cs (head xs) = (head xs) : sonCaminosDeSalida cs (tail xs)
                         | otherwise = sonCaminosDeSalida cs (tail xs)
                         
caminoDeSalidaSinRepetidos :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRepetidos s c = caminoDeSalida s c && (contieneRepe (listaDeMovimientos c) == False) 

--                                          PARTE B



-- Dada una fila toma el elemento en cierta columna (solo para TableroAF)
columnaBAF :: Integer -> [Desplazamiento] -> Desplazamiento
columnaBAF n k | n == 1 = head k
               | otherwise = columnaBAF (n-1) (tail k)
-- Dado un TableroAF toma una cierta fila
filaAAF :: Integer -> [[Desplazamiento]] -> [Desplazamiento]
filaAAF n k | n == 1 = head k
            | otherwise = filaAAF (n-1) (tail k)

-- Hace una lista de movimientos usando la funcion moverse tomando como valor la flecha que hay en la posicion que buscan los auxiliares
recorrido :: TableroAF -> Posicion -> [Posicion]
recorrido n (a,b)|posValida n (a,b) == False = [] 
                 |otherwise = (a,b) :recorrido n (moverse (a,b) (columnaBAF b (filaAAF a n)))

-- Dado un tablero, una posicion y la cantidad de espacios en dicho tablero decide si un AF colocado en dicha posicion escapara del tablero
escapaAux :: TableroAF -> Posicion -> Integer -> Bool
escapaAux k (a,b) n| n == 0 = False
           | posValida k (a,b) == False = True
           |otherwise = escapaAux k (moverse (a,b) (columnaBAF b (filaAAF a k))) (n-1)

-- --Usa la cantidad de cuadrantes del tablero como valor para el Integer  auxiliar, si pasa por todos los cuadrantes del tablero  y no se sale es porque es imposible que salga
escapaDelTablero :: TableroAF -> Posicion -> Bool
escapaDelTablero k (a,b)|escapaAux k (a,b) (tamano k * tamano k) = True
                        |otherwise = False

-- Dado un desplazamiento lo girará 90º en el sentido de las agujas del reloj
cambiaFlecha :: Desplazamiento -> Desplazamiento
cambiaFlecha Arriba = Derecha
cambiaFlecha Derecha = Abajo
cambiaFlecha Abajo = Izquierda
cambiaFlecha Izquierda = Arriba

-- Dado un tablero, una lista de desplazamientos y un numero a reemplaza la fila a del tablero por la lista
reemplazarFila :: TableroAF -> [Desplazamiento] -> Integer -> TableroAF
reemplazarFila k s a| a == 1 = s : (tail k)
                |otherwise =  (head k) : reemplazarFila (tail k) s (a-1)

-- Dada una lista de desplazamientos A, un desplazamiento y un numero b reemplaza el b-esimo elemento de la lista por el desplazamiento dado
reemplazarBEnA :: [Desplazamiento] -> Desplazamiento -> Integer -> [Desplazamiento]
reemplazarBEnA s r b |b == 1 = r : (tail s)
                 |otherwise = (head s) : reemplazarBEnA (tail s) r (b-1)

-- Encuentra una posicion especifica del tablero a traves de los auxiliares y cambia la direccion usando cambiaFlecha
cambiaFlechaTablero :: TableroAF -> Posicion -> TableroAF
cambiaFlechaTablero k (a,b) = reemplazarFila k (reemplazarBEnA (filaAAF a k) (cambiaFlecha (columnaBAF b (filaAAF a k))) b ) a

-- Usa el auxiliar empezando desde el numero cero
cantidadDePasosParaSalir :: TableroAF -> Posicion -> Integer
cantidadDePasosParaSalir k (a,b) = cantidadDePasosParaSalirAux k (a,b) 0

-- Cuenta la cantidad de pasos  que tarda en salir cambiando el cuadrante por el que paso  a traves de cambiaFlechaTablero , se mueve de cuadrante con la funcion moverse  y los auxiliares que encuentran la posicion en el tablero y suma uno al Integer por cada vez que se mueve.Cuando se sale del tablero posValida lo detecta y devuelve la cantidad de cuadrantes que recorrio para salir
cantidadDePasosParaSalirAux :: TableroAF -> Posicion -> Integer -> Integer
cantidadDePasosParaSalirAux k (a,b) n | posValida k (a,b) == False = n
                                      | otherwise = cantidadDePasosParaSalirAux (cambiaFlechaTablero k (a,b)) (moverse (a,b) (columnaBAF b (filaAAF a k))) (n+1)
