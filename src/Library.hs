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
                            altura = max 0 (n-3)}

-- Ejercicio 1 b)--

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

-- Ejercicio 2--

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo.habilidad) jugador


-- Ejercicio 3 a)--

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

data Obstaculo = UnObstaculo {
puedeSuperar :: Tiro -> Bool
, efectoLuegoDeSuperar :: Tiro -> Tiro
}

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal
    | puedeSuperar obstaculo tiroOriginal = efectoLuegoDeSuperar obstaculo tiroOriginal
    | otherwise = tiroEnEstadoDeReposo

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

vaAlRasDelSuelo :: Tiro -> Bool
vaAlRasDelSuelo = (==0).altura

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = tiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}

tiroEnEstadoDeReposo :: Tiro
tiroEnEstadoDeReposo = UnTiro {velocidad = 0, precision = 0, altura = 0}

-- Ejercicio 3 b)--

laguna :: Number -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo}
            
-- Ejercicio 3 c)--

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = between 5 20 (velocidad tiro) && vaAlRasDelSuelo tiro

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroEnEstadoDeReposo

-- Ejercicio 4 a)--

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

-- Ejercicio 4 b)--

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Number
cuantosObstaculosConsecutivosSupera tiro [] = 0
cuantosObstaculosConsecutivosSupera tiro (obstaculo : obstaculos)
    | puedeSuperar obstaculo tiro  
        = 1 + cuantosObstaculosConsecutivosSupera (efectoLuegoDeSuperar obstaculo tiro) obstaculos
    | otherwise = 0

-- Ejercicio 4 c)--

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos 
  = maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos . golpe jugador)       palos

-- Ejercicio 5 --

jugadorDelTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = (map (padre.jugadorDelTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador 
   = (all ((< puntosGanados puntosDeUnJugador) . puntosGanados) . filter (/= puntosDeUnJugador)) puntosDeTorneo

