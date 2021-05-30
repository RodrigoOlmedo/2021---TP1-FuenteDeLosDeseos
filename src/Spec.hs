module Spec where
import PdePreludat
import Library
import Test.Hspec

--Definimos una persona generica para simplificar los casos de prueba
persona0 = UnaPersona{
    edad = 23
,   suenios = [recibirse "medicina", viajar ["tailandia"],queTodoSigaIgual]
,   nombre = "Fransisca"
,   felicidonios = 45
,   habilidades = []
}

persona1 = UnaPersona{
      edad = 20
,   suenios = [recibirse "Radiologo"]
,   nombre = "Maximiliano"
,   felicidonios = 11
,   habilidades = []
}

correrTests :: IO ()
correrTests = hspec $ do

  describe "Coeficiente de Satisfaccion" $ do
    it "El coeficiente de satisfacción de una persona muy feliz es la multiplicacion la edad por sus felicidonios" $ do
      coeficienteDeSatisfaccion (UnaPersona 25 [] "Maria" 101 []) `shouldBe` 2525
    it "El coeficiente de satisfacción de una persona moderadamente feliz es la multiplicacion de la cantidad de suenios por sus felicidonios" $ do
      coeficienteDeSatisfaccion (UnaPersona 24 [recibirse "medicina" ,queTodoSigaIgual] "Juan" 100 []) `shouldBe` 200
    it "El coeficiente de satisfacción de una persona poco feliz es igual a la mitad de sus felicidonios" $ do
      coeficienteDeSatisfaccion (UnaPersona 50 [] "Pedro" 50 []) `shouldBe` 25

  describe "Grado de Ambicion" $ do
    it "El grado de ambición de una persona muy feliz es igual ala multiplicacion de la cantidad de suenios por sus felicidonios" $ do
      gradoDeAmbicion (UnaPersona 26 [recibirse "medicina", viajar ["tailandia"]] "Alf" 101 []) `shouldBe` 202
    it "El grado de ambición de una persona moderadamente feliz es igual a la multiplicacion de su edad por su cantidad de sueños" $ do
      gradoDeAmbicion (UnaPersona 26 [packMedicina, viajar ["tailandia"]] "Alf" 100 []) `shouldBe` 52
    it "El grado de ambición de una persona poco feliz es igual a la multiplicacion de sus sueños por 2" $ do
      gradoDeAmbicion (UnaPersona 26 [recibirse "medicina"] "Alf" 50 []) `shouldBe` 2


  describe "Tiene nombre largo" $ do
    it "Un nombre con menos de 10 letras no es un nombre largo" $ do
      nombreLargo persona0 `shouldBe` False
    it "Un nombre con 10 letras o mas es un nombre largo" $ do
      nombreLargo persona1 `shouldBe` True

  describe "Es una persona suertuda" $ do
    it "Si el triple del coeficiente de satisfaccion de una persona es impar, la misma no es suertuda" $ do
      esPersonaSuertuda persona1 `shouldBe` False
    it "Si el triple del coeficiente de satisfaccion de una persona es par, la persona no es suertuda" $ do
      esPersonaSuertuda persona0 `shouldBe` True

  describe "Tinene un nombre lindo" $ do
    it "Una persona cuyo nombre no termina con a, no tiene nombre lindo" $ do
      nombreLindo persona1 `shouldBe` False
    it "Una persona cuyo nombre termina con a, tiene nombre lindo" $ do
      nombreLindo persona0 `shouldBe` True
 

  describe "Recibirse" $ do
    it "Una persona que se recibe debe recibir 1000 felicidonios por cada letra de la carrera y conseguir la misma como habilidad" $ do
      show (recibirse "Ingenieria" persona1) `shouldBe` show (UnaPersona 20 [recibirse "Radiologo"] "Maximiliano" 10011 ["Ingenieria"])
      --habilidades (recibirse "Ingenieria" persona1) `shouldBe` ["Ingenieria"]
      --felicidonios (recibirse "Ingenieria" persona1) `shouldBe` 10011

  describe "Viajar" $ do
    it "Una persona que viaja debe conseguir 100 felicidonios por cada ciudad que visita y debe crecer un año" $ do
      felicidonios (viajar ["madrid", "barcelona"] persona0) `shouldBe` 245
      edad (viajar ["madrid", "barcelona"] persona0) `shouldBe` 24
    
  describe "Enamorarse" $ do
    it "Una persona enamorada debe sumar la cantidad de felicidonios de la persona de quien se enamoró" $ do
      felicidonios (enamorarse persona1 persona0) `shouldBe` 56

  describe "Todo Sigue Igual" $ do
    it "Una persona que quiere que todo siga igual no debe cambiar" $ do
     show (queTodoSigaIgual persona0) `shouldBe` show persona0

  describe "Pack Medicina" $ do
    it "Una persona que consigue el pack medicinal debe conseguir los efectos de recibirse de medicina, de viajar a Berazategui y Paris y un bonus de 100 felicidonios" $ do
      felicidonios (packMedicina persona0) `shouldBe` 8345
      edad (packMedicina persona0) `shouldBe` 24

  describe "Triplicar Efectos" $ do
    it "Una persona que quiere triplicar un sueño debe recibir los efectos del mismo pero triplicados" $ do
      edad (triple (viajar ["El Chalten"]) persona0) `shouldBe` 26
      felicidonios (triple (viajar ["El Chalten"]) persona0) `shouldBe` 345


  describe "Fuente Minimalista" $ do 
    it "La fuente minimalista debe cumplir el primer sueño de la persona y borrarlo" $ do
      show (fuenteMinimalista persona0) `shouldBe` show (UnaPersona 23 [viajar ["tailandia"],queTodoSigaIgual] "Fransisca" 8045 ["medicina"])

  describe "Fuente Copada" $ do 
    it "La fuente copada debe cumplir todos los sueños de la persona y borrarlos de su lista de sueños" $ do
      show (fuenteCopada persona0) `shouldBe` show (UnaPersona 24 [] "Fransisca" 8145 ["medicina"])

  describe "Fuente A Pedido" $ do 
    it "La fuente a pedido debe cumplir el enesimo sueño de la persona" $ do
      show (fuenteAPedido 1 persona0) `shouldBe` show (UnaPersona 24 [recibirse "medicina", viajar ["tailandia"],queTodoSigaIgual] "Fransisca" 145 [])

  describe "Fuente Sorda" $ do 
    it "La fuente Sorda debe cumplir el enesimo sueño de la persona" $ do
      show (fuenteSorda persona0) `shouldBe` show persona0


  describe "Fuentes Ganadoras" $ do -- Esto esta mal? (por probar todas las fuentes dentro de este item)
    it "Dado un conjunto de fuentes y una persona, la fuente mas satisfactoria será aquella que deje más satisfecha a la persona" $ do
          show ((fuenteMasSatisfactoria persona1 [fuenteSorda, fuenteCopada]) persona1) `shouldBe` show (UnaPersona 20 [] "Maximiliano" 9011 ["Radiologo"])

    it "Dado un conjunto de fuentes y una persona, la fuente mas envejecedora será aquella que mas edad sume a la persona" $ do
          show ((fuenteMasEnvejecedora persona0 [fuenteSorda, fuenteMinimalista, (fuenteAPedido 1)]) persona0) `shouldBe` show (UnaPersona 24 [recibirse "medicina", viajar ["tailandia"],queTodoSigaIgual] "Fransisca" 145 [])

    it "Dado un conjunto de fuentes y una persona, la fuente que mas habilidades da será aquella que deje a la persona con mayor cantidad de habilidades" $ do
          show ((fuenteQueMasHabilidadesDa persona1 [fuenteSorda, fuenteCopada]) persona1) `shouldBe` show (UnaPersona 20 [] "Maximiliano" 9011 ["Radiologo"])


  describe "Sueños Valiosos" $ do
    it "Los sueños valiosos son aquellos que dejan a la persona con mas de 100 felicidonios luego de cumplirlos" $ do
          show (sueniosValiosos persona0) `shouldBe` show ([recibirse "medicina", viajar ["tailandia"]])

  describe "Tiene sueño raro" $ do
    it "Si uno de sus sueños no suma felicidonios, la persona tiene un sueño raro" $ do
          tieneSuenioRaro persona0 `shouldBe` True
    it "Si todos sus sueños suman felicidonios, la persona no tiene un sueño raro," $ do
          tieneSuenioRaro persona1 `shouldBe` False

  describe "Felicidad de un grupo" $ do
    it "La felicidad de un grupo debe ser la suma de la felicidad de cada una de las personas del grupo una vez cumplidos todos sus sueños" $ do
          felicidadDeGrupo [persona0,persona1] `shouldBe` 17156

  describe "Veces a cumplir un sueño para tener mas de mil felicidonios" $ do
    it "La funcion debe contar la cantidad de veces a realizar un sueño para que la persona quede con mas de mil felicidonios" $ do
          vecesACumplirSuenioParaTenerMasDeMilFelicidonios persona0 (viajar ["Quilmes"]) `shouldBe` 10

  