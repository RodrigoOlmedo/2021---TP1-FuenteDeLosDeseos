module Library where
import PdePreludat

--Definimos equivalencias de tipos con el fin de tener un nombres mas representativos

type Suenio = Persona -> Persona
type Fuente = Persona -> Persona
type Ciudad = String 

--modelamos el tipo de dato "Suenio" y permitimos poder mostrarlo en consola y compararlo

--data Suenio = Recibirse | Viajar | Enamorarse | QueTodoSigaIgual | QuierePackMedicina deriving (Show,Eq)

--Definimos el tipo "Persona" utilizando Record Syntax, tipando las caracteristicas de la misma y permitiendo mostrar y comparar a la persona

data Persona = UnaPersona{
    edad :: Number
,   suenios :: [Suenio]
,   nombre :: String
,   felicidonios :: Number
,   habilidades :: [String]
}deriving (Show,Eq)

pepe = UnaPersona{
    edad = 23
,   suenios = [recibirse "medicina", viajar ["tailandia"]]
,   nombre = "Fransisca"
,   felicidonios = 45
,   habilidades = []
}
-- 1 a 

esMuyFeliz :: Persona -> Bool
esMuyFeliz = (>100).felicidonios

esModeradamenteFeliz :: Persona -> Bool
esModeradamenteFeliz persona = felicidonios persona >50 && felicidonios persona <=100
--esModeradamenteFeliz = (>50).felicidonios se podria hacer esto porque no entraria a la condicion en la guarda, pero no conviene como concepto

esPocoFeliz :: Persona -> Bool
esPocoFeliz = (<50).felicidonios


coeficienteDeSatisfaccion :: Persona -> Number
coeficienteDeSatisfaccion persona  | esMuyFeliz persona = felicidonios persona * edad persona
                                   | esModeradamenteFeliz persona =cantidadDeSuenios persona * felicidonios persona 
                                   | otherwise = div (felicidonios persona) 2 

--1b 
gradoDeAmbicion :: Persona ->  Number
gradoDeAmbicion persona | esMuyFeliz persona = felicidonios persona * cantidadDeSuenios persona
                        | esModeradamenteFeliz persona = (edad persona)* cantidadDeSuenios persona
                        | otherwise = cantidadDeSuenios persona * 2

cantidadDeSuenios :: Persona->Number
cantidadDeSuenios = length.suenios

{- Respuestas c

        - Son los tres estados de felicidad en la cual se puede encontrar una persona, dependiendo su cantidad de felicidonios. Son las 
        Clases de Equivalencia que puede adoptar la funcion.

        - Los casos de prueba estan hechos en base a las tres situaciones que se pueden dar dentro de las funciones
        coeficienteDeSatisfaccion y gradoDeAmbicion, al tomar un valor dentro de cada rango podemos comprobar que, sin importar el caso,
        toda la funcion funciona (valga la redundancia) correctamente. Ademas, tomamos los valores límites
        donde la función cambia su comportamiento y es mas probable que se rompa.
          
-}

-- 2 a 
nombreLargo :: Persona -> Bool
nombreLargo = (>10).length.nombre


-- 2 b 
esPersonaSuertuda :: Persona -> Bool
esPersonaSuertuda =  even.(3*).coeficienteDeSatisfaccion --aplicacion parcial y composicion
 


-- (Error 404: Tercer Integrante not found)

terminaEnA :: String->Bool
terminaEnA = ('a'==).last -- aplicacion parcial y composicion (Debate acerca de que tan util es si cambia el parametro)

nombreLindo :: Persona -> Bool
nombreLindo = terminaEnA.nombre

--3 PARTE A
agregarFelicidonios :: Number->Persona -> Persona
agregarFelicidonios felicidoniosASumar persona= persona {felicidonios = felicidonios persona + felicidoniosASumar}

agregarHabilidad :: String -> Persona -> Persona
agregarHabilidad habilidad persona= persona {habilidades = habilidad :habilidades persona}

sumarAnios :: Number -> Persona -> Persona
sumarAnios anios persona = persona {edad=edad persona+anios}

recibirse :: String -> Suenio 
recibirse carrera = (agregarHabilidad carrera).(agregarFelicidonios (1000*length carrera))

viajar :: [Ciudad]->Suenio
viajar ciudades = (sumarAnios 1).(agregarFelicidonios (100*cantidadDeCiudades ciudades)) 

cantidadDeCiudades :: [Ciudad]->Number 
cantidadDeCiudades = length

--(Error 404: Tercer Integrante not Found)

enamorarse :: Persona -> Suenio
enamorarse personaY  = (agregarFelicidonios (felicidonios personaY))

-- 3 PARTE B

queTodoSiguaIgual :: Suenio
queTodoSiguaIgual = id--no cambia

packMedicina :: Suenio
packMedicina = bonus.viajeMedicinal.esMedico --composicion

esMedico :: Suenio --se recibe de medicina
esMedico = recibirse "Medicina"

viajeMedicinal :: Suenio --viaja a 2 ciudades
viajeMedicinal = viajar ["Berazategui","Paris"]

bonus :: Persona->Persona --Suma 100 felicidonios
bonus = agregarFelicidonios 100

triple :: Suenio -> Suenio 
triple suenio = suenio.suenio.suenio

fuenteMinimalista :: Fuente
fuenteMinimalista persona = sacarPrimerSuenio ((enesimoSuenio 0 persona) persona)

--primerSuenio persona = head suenios persona
--cumplirSuenio suenio = suenio QUIERO USAR COMPOSICION EN FUENTEMINIMALISTA Y NO SE COMO :(

enesimoSuenio :: Number->Persona -> Suenio
enesimoSuenio numero persona = (!!) (suenios persona) numero

sacarPrimerSuenio :: Persona->Persona
sacarPrimerSuenio persona = persona{suenios = tail (suenios persona)}

fuenteSorda :: Fuente
fuenteSorda = queTodoSiguaIgual

fuenteGanadoraSegun :: (Persona->Number)->