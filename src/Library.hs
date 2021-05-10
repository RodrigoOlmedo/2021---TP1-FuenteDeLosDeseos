module Library where
import PdePreludat

--Definimos equivalencias de tipos con el fin de tener un nombres mas representativos

type SuenioCumplido = Persona -> Persona
type PersonaModificada = Persona -> Persona
type Ciudad = String 

--modelamos el tipo de dato "Suenio" y permitimos poder mostrarlo en consola y compararlo

data Suenio = Recibirse | Viajar | Enamorarse | QueTodoSigaIgual | QuierePackMedicina deriving (Show,Eq)

--Definimos el tipo "Persona" utilizando Record Syntax, tipando las caracteristicas de la misma y permitiendo mostrar y comparar a la persona

data Persona = UnaPersona{
    edad :: Number
,   suenios :: [Suenio]
,   nombre :: String
,   felicidonios :: Number
,   habilidades :: [String]
}deriving (Show,Eq)


-- 1 a 

coeficienteDeSatisfaccion :: Persona -> Number
coeficienteDeSatisfaccion persona 
    | felicidonios persona > 100 = felicidonios persona * edad persona
    | felicidonios persona <= 50 = div (felicidonios persona) 2 
    | otherwise = cantidadDeSuenios (suenios persona) * felicidonios persona


{- A ESTO NO LE DES BOLA QUE NO ESTA EN EL CODIGO, LO QUISE HACER ASI PERO NO ENTENDI COMO RESOLVERLO


data Felicidad = MuyFeliz  ModeradamenteFeliz  PocoFeliz deriving (Eq)

coeficienteDeSatisfaccion  Persona - Number
coeficienteDeSatisfaccion persona 
     felicidad persona == MuyFeliz = felicidonios persona  edad persona
     felicidad persona == PocoFeliz = div (felicidonios persona) 2 
     otherwise = cantidadDeSuenios (suenios persona)  felicidonios persona

esMuyFeliz  Persona - Felicidad
esMuyFeliz persona = felicidonios persona 100 

felicidad  Persona - Felicidad
felicidad persona  felicidonios persona 100 = MuyFeliz
                   felicidonios persona =50 = PocoFeliz
                   otherwise = ModeradamenteFeliz 
-}    

--1b 
gradoDeAmbicion :: Persona ->  Number
gradoDeAmbicion persona | felicidonios persona > 100 = felicidonios persona * cantidadDeSuenios (suenios persona)
                        | felicidonios persona <= 100 && felicidonios persona > 50 = (edad persona)* cantidadDeSuenios (suenios persona)
                        | otherwise = cantidadDeSuenios (suenios persona) * 2

cantidadDeSuenios :: [Suenio]->Number
cantidadDeSuenios = length

{- Respuestas c

        - Son los tres estados de felicidad en la cual se puede encontrar una persona, dependiendo su cantidad de felicidonios

        - Los casos de prueba estan hechos en base a las tres situaciones que se pueden dar dentro de las funciones
        coeficienteDeSatisfaccion y gradoDeAmbicion, al tomar un valor dentro de cada rango podemos comprobar que, sin importar el caso,
        toda la funcion funciona (valga la redundancia) correctamente 
          
-}

-- 2 a 
nombreLargo :: Persona -> Bool
nombreLargo persona = length (nombre persona) > 10


-- 2 b 
boolPersonaSuertuda :: Persona -> Bool
boolPersonaSuertuda =  even.(3*).coeficienteDeSatisfaccion --aplicacion parcial y composicion
 

personaSuertuda :: Persona -> String
personaSuertuda persona | boolPersonaSuertuda persona == True = "Es suertuda"
                        | boolPersonaSuertuda persona == False = "No es suertuda"


-- (Error 404: Tercer Integrante not found)

terminaEnA :: String->Bool
terminaEnA = ('a'==).last -- aplicacion parcial y composicion (Debate acerca de que tan util es si cambia el parametro)

nombreLindo :: Persona -> Bool
nombreLindo persona = terminaEnA (nombre persona)

--3 PARTE A

seRecibio :: String -> PersonaModificada -- se recibe pero no necesariamente es su sueño 
seRecibio carrera persona = cumplioRecibirse carrera (habilidadExistente carrera persona) (sueniaRecibirse persona) persona

sueniaRecibirse :: Persona -> Bool --Evaluamos si su suenio es recibirse
sueniaRecibirse persona =  elem Recibirse (suenios persona)||elem QuierePackMedicina (suenios persona)

cumplioRecibirse :: String -> Bool -> Bool -> SuenioCumplido --Si cada sueño fuese validado, definiriamos: SuenioCumplido = Bool->Persona->Persona
cumplioRecibirse carrera condicion False persona = persona  --Utilizamos Pattern Matching para evaluar condiciones
cumplioRecibirse carrera False True persona  = UnaPersona {
        edad = edad persona
    ,   suenios = suenios persona
    ,   nombre = nombre persona
    ,   felicidonios = felicidonios persona + 1000 * length carrera
    ,   habilidades = carrera:habilidades persona }

cumplioRecibirse carrera True True persona  = UnaPersona {
        edad = edad persona
    ,   suenios = suenios persona
    ,   nombre = nombre persona
    ,   felicidonios = felicidonios persona + 1000 * length carrera
    ,   habilidades = habilidades persona }

habilidadExistente :: String -> Persona -> Bool --comprobamos si tiene como habilidad la carrera
habilidadExistente carrera persona = elem carrera (habilidades persona)


viajo :: [Ciudad]->SuenioCumplido
viajo ciudades persona= UnaPersona{
    edad = edad persona +1
,   suenios = suenios persona
,   nombre = nombre persona
,   felicidonios = felicidonios persona + 100 * cantidadDeCiudades ciudades
,   habilidades = habilidades persona
    
} 

cantidadDeCiudades :: [Ciudad]->Number 
cantidadDeCiudades = length

--(Error 404: Tercer Integrante not Found)

seEnamoro :: Persona -> SuenioCumplido
seEnamoro personaY personaX = UnaPersona{
    edad = edad personaX
,   suenios = suenios personaX
,   nombre = nombre personaX
,   felicidonios = felicidonios personaX + felicidonios personaY --sumamos felicidonios de la otra persona
,   habilidades = habilidades personaX
}

-- 3 PARTE B

todoSigueIgual :: SuenioCumplido
todoSigueIgual persona = persona --no cambia

packMedicina :: SuenioCumplido
packMedicina = bonus.viajeMedicinal.esMedico --composicion

esMedico :: PersonaModificada --se recibe de medicina
esMedico = seRecibio "Medicina"

viajeMedicinal :: PersonaModificada --viaja a 2 ciudades
viajeMedicinal = viajo ["Berazategui","Paris"]

bonus :: PersonaModificada --Suma 100 felicidonios
bonus persona = UnaPersona{
    edad = edad persona
,   suenios = suenios persona
,   nombre = nombre persona
,   felicidonios = felicidonios persona + 100
,   habilidades = habilidades persona
}

triple :: Suenio-> [Ciudad] -> String -> Persona -> SuenioCumplido 
triple QuierePackMedicina _ _ _= packMedicina.packMedicina.packMedicina --Utilizamos Pattern Matching para definir el sueño a triplicar
triple QueTodoSigaIgual _ _ _= todoSigueIgual.todoSigueIgual.todoSigueIgual
triple Enamorarse _ _ personaB = (seEnamoro personaB).(seEnamoro personaB).(seEnamoro personaB)
triple Recibirse _ carrera _ = (seRecibio carrera).(seRecibio carrera).(seRecibio carrera)
triple Viajar suenios _ _ = (viajo suenios).(viajo suenios).(viajo suenios)