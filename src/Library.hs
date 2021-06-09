module Library where
import PdePreludat

data Jugador = UnJugador {
nombre :: String
, padre :: String
, habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
fuerzaJugador :: Number
, precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador {
nombre = "Bart"
, padre = "Homero"
, habilidad = Habilidad {fuerzaJugador = 25, precisionJugador = 60} 
}

todd = UnJugador {
nombre = "Todd"
, padre = "Ned"
, habilidad = Habilidad {fuerzaJugador = 15, precisionJugador = 80}
}

rafa = UnJugador {
nombre = "Rafa"
, padre = "Gorgory"
, habilidad = Habilidad {fuerzaJugador = 10, precisionJugador = 1} 
}

data Tiro = UnTiro {
velocidad :: Number
, precision :: Number
, altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funcines útiles

between :: Eq a => Enum a => a -> a -> a -> Bool
between n m x = elem x [n..m]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord b => (a -> b) -> a -> a -> a
mayorSegun f a b
    | f a > f b = a
    | otherwise = b

-- Ejercicio 1 a)--

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {velocidad = 10, precision = 2 * (precisionJugador habilidad), altura = 0}

madera :: Palo
madera habilidad = UnTiro {velocidad = 100, precision = (precisionJugador habilidad) / 2, altura = 5}

hierro :: Number -> Palo
hierro n habilidad = UnTiro {velocidad = (fuerzaJugador habilidad) * n, precision = (precisionJugador habilidad) / n, 
                            altura = n - 3}

-- Ejercicio 1 b)--

palos :: [Palo]
palos = [putter, madera, hierro 1, hierro 2, hierro 3, hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

-- Ejercicio 2--

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

-- Ejercicio 3 a)--

type Obstaculo = Tiro -> Bool

tunelConRampita :: Obstaculo
tunelConRampita tiro = precision tiro > 90 && altura tiro == 0 

tiroDespuesDelTunel :: Tiro -> Tiro
tiroDespuesDelTunel tiro 
            | tunelConRampita tiro = tiro {velocidad = (velocidad tiro) * 2, precision = 100, altura = 0}
            | otherwise = tiroEnEstadoDeReposo

tiroEnEstadoDeReposo :: Tiro
tiroEnEstadoDeReposo = UnTiro {velocidad = 0, precision = 0, altura = 0}

-- Ejercicio 3 b)--

laguna :: Obstaculo
laguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

type Largo = Number

tiroDespuesDeLaLaguna :: Largo -> Tiro -> Tiro
tiroDespuesDeLaLaguna largoDeLaguna tiro 
            | laguna tiro = tiro { altura = (altura tiro) / largoDeLaguna}
            | otherwise = tiroEnEstadoDeReposo

-- Ejercicio 3 c)--

hoyo :: Obstaculo
hoyo tiro = between 5 20 (velocidad tiro) && altura tiro == 0 && precision tiro > 95

tiroDespuesDelHoyo :: Tiro -> Tiro
tiroDespuesDelHoyo tiro = tiroEnEstadoDeReposo

-- Ejercicio 4 a)--

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (condicion jugador obstaculo) palos

condicion :: Jugador -> Obstaculo -> Palo -> Bool
condicion jugador obstaculo palo = obstaculo (golpe jugador palo)

-- Ejercicio 4 b)--
