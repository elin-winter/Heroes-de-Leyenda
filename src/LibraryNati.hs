module LibraryNati where
import PdePreludat

type Reconocimiento = Number
type Artefactos = [Artefacto]
type Rareza = Number
type Epiteto = String
type Nombre = String
type Cuadras = Number
type Debilidad = Heroe -> Bool 
type Tarea = Heroe -> Heroe

data Heroe = UnHeroe{
    reconocimiento :: Reconocimiento,
    epiteto :: Epiteto,
    artefactos :: Artefactos,
    tareas :: [Tarea]
} deriving (Show, Eq)

heracles :: Heroe
heracles = UnHeroe {
    reconocimiento = 700,
    epiteto = "Guardian del Olimpo",
    artefactos = [pistola, relampagoZeus],
    tareas = [matarAlLeonNemea]
}

data Artefacto = UnArtefacto {
    nombreArtefacto :: Nombre,
    rareza :: Rareza
}deriving (Show, Eq)

lanzaOlimpo :: Artefacto
lanzaOlimpo = UnArtefacto{
    nombreArtefacto = "Lanza del Olimpo",
    rareza = 100
}

xiphos :: Artefacto
xiphos = UnArtefacto{
    nombreArtefacto = "Xiphos",
    rareza = 50
}

relampagoZeus :: Artefacto
relampagoZeus = UnArtefacto{
    nombreArtefacto = "Relampago de Zeus",
    rareza = 500
}

pistola :: Artefacto
pistola = UnArtefacto {
    nombreArtefacto = "Pistola",
    rareza = 1000
}

data Bestia = UnaBestia {
    nombreBestia :: Nombre,
    debilidad :: Debilidad
}deriving (Show, Eq)

leonNemea :: Bestia
leonNemea = UnaBestia{
    nombreBestia = "Leon de Nemea",
    debilidad = debilidadNemea
}
--Funcion 1

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto artefacto heroe  = heroe {artefactos = (artefactos heroe) ++ [artefacto]}

modificarEpiteto :: Epiteto -> Heroe  -> Heroe
modificarEpiteto nuevoEpiteto heroe = heroe{epiteto = nuevoEpiteto}

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria heroe
    |reconocimiento heroe > 1000 = modificarEpiteto "El Mítico" heroe
    |reconocimiento heroe >= 500 = (modificarEpiteto "El Magnífico" . agregarArtefacto lanzaOlimpo) heroe
    |reconocimiento heroe > 100 = (modificarEpiteto "Hoplita" . agregarArtefacto xiphos) heroe
    |otherwise = heroe

-- Funcion 2

--Tarea 1
obtenerRareza :: Artefacto -> Rareza
obtenerRareza = rareza

modificarReconocimiento :: Number -> Heroe -> Heroe
modificarReconocimiento x heroe = heroe {reconocimiento = reconocimiento heroe + x}

encontrarArtefacto ::Artefacto -> Tarea
encontrarArtefacto artefacto = modificarReconocimiento (obtenerRareza artefacto) . agregarArtefacto artefacto

-- Tarea 2

triplicarRarezaArtefacto :: Artefacto  -> Artefacto
triplicarRarezaArtefacto artefacto = artefacto{rareza = (rareza artefacto) * 3}

triplicarRarezaArtefactos :: Artefactos  -> Artefactos
triplicarRarezaArtefactos = map triplicarRarezaArtefacto  

desecharNoAlcanzanMinimoRareza :: Artefactos  -> Artefactos
desecharNoAlcanzanMinimoRareza = filter ((>1000) . rareza) 

obtenerArtefactosFiltrados :: Artefactos  -> Artefactos
obtenerArtefactosFiltrados  = desecharNoAlcanzanMinimoRareza . triplicarRarezaArtefactos 

actualizarArtefactos ::  Heroe -> Heroe
actualizarArtefactos heroe = heroe { artefactos = obtenerArtefactosFiltrados (artefactos heroe)}

escalarOlimpo :: Tarea
escalarOlimpo = agregarArtefacto relampagoZeus . actualizarArtefactos . modificarReconocimiento 500

--Tarea 3

ayudaACruzarCalle  :: Cuadras  ->  Tarea
ayudaACruzarCalle cuadras heroe = heroe {epiteto = armarGroso cuadras}

armarGroso :: Cuadras  -> Epiteto
armarGroso cuadras = "Gros" ++ replicate cuadras 'o'

-- Tarea 4

epitetoBestia :: Bestia -> Epiteto
epitetoBestia bestia = "El asesino de la " ++ (nombreBestia bestia)

perderArtefactos :: Heroe  -> Heroe
perderArtefactos  heroe = heroe{artefactos = drop 1 (artefactos heroe)}

matarBestia :: Bestia -> Tarea
matarBestia bestia heroe
    |(debilidad bestia) heroe = modificarEpiteto (epitetoBestia bestia) heroe
    |otherwise = (perderArtefactos . modificarEpiteto "El Cobarde") heroe

-- Tarea 5

debilidadNemea :: Debilidad
debilidadNemea = (>20) . length . epiteto

matarAlLeonNemea :: Tarea
matarAlLeonNemea = matarBestia leonNemea 

-- Funcion 3

hacerTarea :: Heroe -> Tarea -> Heroe
hacerTarea heroe tarea = tarea heroe  

-- Funcion 4

rarezasArtefactos :: Artefactos -> [Rareza]
rarezasArtefactos = map rareza 

sumRarezas :: Heroe -> Number
sumRarezas = sum . rarezasArtefactos .artefactos 

presumirLogros :: Heroe -> Heroe -> (Heroe, Heroe)
presumirLogros heroe1 heroe2
    |reconocimiento heroe1 > reconocimiento heroe2 = (heroe1, heroe2)
    |reconocimiento heroe2 > reconocimiento heroe1 = (heroe2, heroe1)
    |sumRarezas heroe1 > sumRarezas heroe2 = (heroe1, heroe2)
    |sumRarezas heroe2 > sumRarezas heroe1 = (heroe2, heroe1)
    |otherwise = presumirLogros (realizarTareasDelOtro heroe1 heroe2) (realizarTareasDelOtro heroe2 heroe1)

realizarTareasDelOtro :: Heroe  -> Heroe  -> Heroe
realizarTareasDelOtro heroe1 heroe2 = foldl hacerTarea heroe1 (tareas heroe2)

{-
-- Ej 8) ¿Cuál es el resultado de hacer que presuman dos héroes con reconocimiento 100, ningún artefacto y
ninguna tarea realizada?

En primer lugar, comparará los reconocimientos y, como son iguales, continuará con la evaluacion de los artefactos. No obstante, como estas son vacias, procederá a hacer las tareas del otro. Como ninguno tiene tareas, siempre se devolverá a los mismos heroes, por lo que continuará evaluando indefinidamente.

-}

--Funcion 5

realizarLabor :: [Tarea] -> Heroe -> Heroe
realizarLabor tareas heroe = foldl hacerTarea heroe tareas

{-
10) Si invocamos la función anterior con una labor infinita,
¿se podrá conocer el estado final del héroe? ¿Por qué?

No, ya que se continuaría aplicando las tareas infinitamente ya que fold requiere aplicar todos los elementos de la lista para devolver el resultado de la evaluación.

-}

