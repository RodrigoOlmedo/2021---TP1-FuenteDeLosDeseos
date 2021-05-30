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

queTodoSigaIgual :: Suenio
queTodoSigaIgual = id--no cambia

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

------------------------------ 2DA ENTREGA 
{-
--Modelamos de otra manera a los suenios de la persona, dado que de no ser asi las fuentes no podrian cumplir los deseos segun su lista:

fuenteMinimalista :: Fuente
fuenteMinimalista persona = suenioEsTalFuncion.(flip enesimoSuenio 0)) persona) persona

suenioEsTalFuncion :: Suenio -> SuenioCumplido
suenioEsTalFuncion Recibirse = recibirse --No podria devolver un "SuenioCumplido" siendo que le falta el String, y no le puedo pasar el String
suenioEsTalFuncion Viajar = viajar       --Dado que en esta le deberia pasar una lista de Strings, si lo modelasemos con los

constructores del Data deberiamos haber puesto como constructores "Recibirse_Medicina", pero esto da lugar a infinitas posibilidades.
Con lo cual -> Imposible
-}

--4a Int1

fuenteMinimalista :: Fuente
fuenteMinimalista persona = (sacarPrimerSuenio.(cumplirUnSuenio (enesimoSuenio persona 0))) persona

enesimoSuenio :: Persona->Number-> Suenio
enesimoSuenio persona = (!!) (suenios persona)

sacarPrimerSuenio :: Persona->Persona
sacarPrimerSuenio persona = persona{suenios = tail (suenios persona)}

--4b Int2
fuenteCopada :: Fuente
fuenteCopada = borrarSuenios.cumplirTodosLosSuenios

cumplirUnSuenio :: Suenio -> Persona -> Persona
cumplirUnSuenio suenio = suenio

cumplirTodosLosSuenios :: Persona -> Persona
cumplirTodosLosSuenios persona = foldl (flip cumplirUnSuenio) persona (suenios persona)

borrarSuenios :: Persona -> Persona
borrarSuenios persona = persona {suenios = []}

--4c
fuenteAPedido :: Number -> Fuente
fuenteAPedido numero persona = cumplirUnSuenio (enesimoSuenio persona numero) persona

--4d todo el grupo
fuenteSorda :: Fuente
fuenteSorda = id

----------------

--5
fuenteGanadoraSegun :: (Persona->Number) -> Persona->[Fuente]->Fuente
fuenteGanadoraSegun indice persona = foldl1 (mejorFuenteSegun indice persona) 
--hacemos aplicacion parcial con el indice y la persona para que sea una funcion del tipo Fuente->Fuente->Fuente

mejorFuenteSegun :: (Persona->Number) -> Persona -> Fuente -> Fuente -> Fuente
mejorFuenteSegun indice persona fuente1 fuente2 | indice (fuente1 persona)>indice (fuente2 persona) = fuente1
                                                | otherwise = fuente2

--5.Int1
fuenteMasSatisfactoria :: Persona -> [Fuente] -> Fuente
fuenteMasSatisfactoria persona = fuenteGanadoraSegun coeficienteDeSatisfaccion persona

--5.Int2
fuenteMasEnvejecedora :: Persona -> [Fuente] -> Fuente
fuenteMasEnvejecedora persona = fuenteGanadoraSegun edad persona

--5.3
fuenteQueMasHabilidadesDa :: Persona -> [Fuente] -> Fuente
fuenteQueMasHabilidadesDa persona = fuenteGanadoraSegun (length.habilidades) persona

---------------------------

-- 6 Int1
--Saber los suenios valiosos, son aquellos que dejan a la persona con mas de 100 felicidonios
sueniosValiosos :: Persona -> [Suenio]
sueniosValiosos persona = filter (flip esSuenioValioso persona) (suenios persona)

esSuenioValioso :: Suenio -> Persona ->Bool
esSuenioValioso suenio = esMuyFeliz.suenio

--6 Int2
--Saber si algún sueño de una persona es raro: La deja con la misma cantidad de felicidonios tras cumplirlo.
tieneSuenioRaro :: Persona -> Bool
tieneSuenioRaro persona = any (esSuenioRaro persona) (suenios persona) --MODIFICADO

esSuenioRaro:: Persona -> Suenio -> Bool 
esSuenioRaro persona suenio = felicidonios persona == felicidonios (suenio persona)

--6.3
--Dada una lista de personas, poder conocer la felicidad total de ese grupo si cumplen todos sus sueños.
felicidadDeGrupo :: [Persona]->Number
felicidadDeGrupo = sum.(map felicidonios).(map cumplirTodosLosSuenios)

--6 Todo el grupo
--Saber cuántas veces debe cumplir un sueño una persona para que quede con más de 1000 felicidonios.

vecesACumplirSuenioParaTenerMasDeMilFelicidonios :: Persona->Suenio->Number
vecesACumplirSuenioParaTenerMasDeMilFelicidonios persona = (cuentaSueniosSegun ((>1000).felicidonios) 0 persona) 
--Rompe con que todo siga igual porque no suma felicidonios

cuentaSueniosSegun :: (Persona->Bool)->Number->Persona ->Suenio -> Number 
cuentaSueniosSegun condicion numero persona suenio  | condicion persona = numero  
                                                    | otherwise = cuentaSueniosSegun condicion (numero+1) (suenio persona) suenio 
-- primera guarda: esSuenioRaro persona suenio = error "No suben felicidonios" esto sería si la funcion solo evaluase los felicidonios

------------

--7
soniador = UnaPersona{
    edad = 23
,   suenios = repeat (recibirse "medicina")
,   nombre = "Francisca"
,   felicidonios = 45
,   habilidades = []
}

--Es posible que la fuente pueda utilizarse con una persona que tiene infinitos sueños? Justifique su respuesta 

--Integrante1 
--Es posible. Si bien soniador es una persona con infinitos sueños al aplicar la Fuente minimalista con esta persona, la misma solo se enfoca 
--en el primer sueño de su lista de sueños, dejando sin importancia el resto. Esto se debe a que Haskell evalua mediante "lazy evaluation"
-- lo cual es un metodo de evaluacion que permite no realizar calculos de expresiones que sean innecesarios.

--Integrante2. 
--No es posible. Dado que la persona tiene una lista de sueños infinitos, y al aplicar la fuenteCopada se quedaría aplicando la 
--función "cumplirTodosLosSuenios" a la lista y no terminaría nunca. 

--Integrante3
--Es posible. Ya que a pesar de que soniador es una persona con infinitos sueños, al aplicar la Fuente a pedido, la misma solo se enfoca  
--en un sueño que tiene una posicion particular en su lista de sueños. Cuando la funcion enesimosSueños encuentre tal posicion 
--dejará sin importancia el resto. Esto se debe a que Haskell evalua mediante "lazy evaluation" lo cual es un metodo de evaluacion que  
--permite no realizar calculos de expresiones que sean innecesarios.