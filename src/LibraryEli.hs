module LibraryEli where
import PdePreludat


-- ---------------------------- Dominio ------------------------

data Heroe = UnHeroe{
    epiteto :: Epiteto,
    reconocimiento :: Reconocimiento,
    artefactos :: [Artefacto],
    listaTareas :: [Tarea]
}

data Artefacto = UnArtefacto {
    nombreArtefacto :: Nombre,
    rareza :: Rareza
}

data Bestia = UnaBestia {
    nombreBestia :: Nombre,
    debilidad :: Debilidad
}

-- ---------------------------- Definición de Tipos ------------------------

type Epiteto = String
type Nombre = String
type Reconocimiento = Number
type Rareza = Number
type Debilidad = Heroe -> Bool

type Tarea = Heroe -> Heroe
type Labor = [Tarea]
-- ---------------------------- Ejemplos ------------------------

lanza :: Artefacto
lanza = UnArtefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = UnArtefacto "Xiphos" 50

relampago :: Artefacto
relampago = UnArtefacto "Relampago de Zeus" 500

pistola :: Artefacto
pistola = UnArtefacto "Pistola Griega" 1000

heroeNulo :: Heroe
heroeNulo = UnHeroe [] 0 [] []

-- ---------------------------- Parte 1 ------------------------
-- Funcion 2
pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria heroe 
    | reconocimiento heroe > 1000 = cambiarEpiteto "El Mitico" heroe
    | reconocimiento heroe >= 500 = (cambiarEpiteto "El Magnifico" . agregarArtefacto lanza) heroe
    | reconocimiento heroe > 100 = (cambiarEpiteto "Hoplita" . agregarArtefacto xiphos) heroe
    | otherwise = heroe
    
    
cambiarEpiteto :: Epiteto -> Heroe -> Heroe
cambiarEpiteto newEpiteto heroe = heroe{ epiteto = newEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto artefacto heroe = heroe{ 
    artefactos = artefacto : artefactos heroe
    }

-- ---------------------------- Parte 2 ------------------------
-- --------------- Punto 1
-- Tarea 1
encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto artefacto = cambiarReco (rareza artefacto). agregarArtefacto artefacto

cambiarReco :: Number -> Heroe -> Heroe
cambiarReco delta heroe = heroe{ 
    reconocimiento = reconocimiento heroe + delta
    } 

-- Tarea 2
escalarOlimpo :: Tarea
escalarOlimpo = agregarArtefacto relampago . cambiarReco 500 . cambiarArtefactos

cambiarArtefactos :: Heroe -> Heroe
cambiarArtefactos heroe = heroe {
    artefactos = cambiarRareza (artefactos heroe)
}

cambiarRareza :: [Artefacto] -> [Artefacto]
cambiarRareza artefactos = filter ((>1000) . rareza) (artefactosCambiados artefactos)

artefactosCambiados :: [Artefacto] -> [Artefacto]
artefactosCambiados = map triplicarRareza 

triplicarRareza :: Artefacto -> Artefacto
triplicarRareza artefacto = artefacto{ rareza = rareza artefacto *3}

-- Tarea 3
ayudaACruzarCalle :: Number -> Tarea
ayudaACruzarCalle delta = cambiarEpiteto (generarGroso delta) 

generarGroso :: Number -> Epiteto
generarGroso delta = "Gros" ++ replicate delta 'o'

-- Tarea 4
matarBestia :: Bestia -> Tarea
matarBestia bestia heroe 
    | aprovechaDebilidad heroe bestia = cambiarEpiteto (epitetoBestia bestia) heroe
    | otherwise = (pierdeArtefacto . cambiarEpiteto "El Cobarde") heroe

epitetoBestia :: Bestia -> Epiteto
epitetoBestia bestia = "El asesino de la " ++ (nombreBestia bestia)

pierdeArtefacto :: Heroe -> Heroe
pierdeArtefacto heroe = heroe {artefactos = drop 1 (artefactos heroe)}

aprovechaDebilidad :: Heroe -> Bestia -> Bool
aprovechaDebilidad heroe bestia = (debilidad bestia) heroe

-- --------------- Punto 2

heracles :: Heroe
heracles = UnHeroe "Guardián del Olimpo" 700 [relampago, pistola] [matarBestia leon]

-- --------------- Punto 5 ¿?
leon :: Bestia
leon = UnaBestia "Leon de Nemea" (epitetoMayor 20)

epitetoMayor :: Number -> Debilidad
epitetoMayor delta heroe = length (epiteto heroe) >= delta

-- --------------- Punto 6
hacerTarea :: Heroe -> Tarea -> Heroe
hacerTarea heroe tarea = tarea heroe

-- --------------- Punto 7
presumirLogros :: Heroe -> Heroe -> (Heroe, Heroe)
presumirLogros heroe1 heroe2 
    | compararProp heroe1 heroe2 reconocimiento = (heroe1, heroe2)
    | compararProp heroe2 heroe1 reconocimiento = (heroe2, heroe1)
    | compararProp heroe1 heroe2 sumaRarezas = (heroe1, heroe2)
    | compararProp heroe2 heroe1 sumaRarezas = (heroe2, heroe1)
    | tareasNulas heroe1 heroe2 = (heroeNulo , heroeNulo)
    | otherwise = presumirLogros (hacerTareaOtro heroe1 heroe2) (hacerTareaOtro heroe2 heroe1) 

compararProp :: Heroe -> Heroe -> (Heroe -> Number) -> Bool
compararProp heroe1 heroe2 prop = (prop heroe1) < (prop heroe2)

sumaRarezas :: Heroe -> Number 
sumaRarezas =  sum . map rareza . artefactos

tareasNulas :: Heroe -> Heroe -> Bool
tareasNulas heroe1 heroe2 = (null . listaTareas) heroe1 && (null . listaTareas) heroe2

hacerTareaOtro :: Heroe -> Heroe -> Heroe
hacerTareaOtro heroeQueHace heroeConTareas = foldl hacerTarea heroeQueHace (listaTareas heroeConTareas)

-- --------------- Punto 8
{-
8) ¿Cuál es el resultado de hacer que presuman dos héroes con 
reconocimiento 100, ningún artefacto y ninguna tarea realizada?

En mi caso, devuelve una tupla de dos heroes nulos, ya que no se puede
calcular cuál de los dos gana la contienda, entonces ninguno gana, ni ninguno
pierde. 

-}

-- --------------- Punto 9

hacerLabor :: Labor -> Heroe -> Heroe
hacerLabor labor heroe = foldl hacerTarea heroe labor

-- --------------- Punto 10
{-
10) Si invocamos la función anterior con una labor infinita,
¿se podrá conocer el estado final del héroe? ¿Por qué?

No, no se podría conocer el estado final del heroe, ya que esta funcion
no aprovecha la lazy evaluation de Haskell. No importa que el lenguaje primero
tome en cuenta las funciones y luego los parametros especificos porque, para este caso,
debe aplicarle al heroe cada una de las tareas, y al ser estas infinitas,
nunca podría terminar de aplicarlas. 

-}