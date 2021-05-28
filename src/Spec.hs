module Spec where
import PdePreludat
import Library
import Test.Hspec

--Definimos una persona generica para simplificar los casos de prueba
pepe = UnaPersona{
    edad = 23
,   suenios = [recibirse "medicina", viajar ["tailandia"],queTodoSigaIgual]
,   nombre = "Fransisca"
,   felicidonios = 45
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

  describe "Es un Nombre Largo" $ do
    it "Un nombre con menos de 10 letras no es un nombre largo" $ do
      nombreLargo (UnaPersona 26 [packMedicina, queTodoSigaIgual] "Evangelina" 101 []) `shouldBe` False
    it "Un nombre con 10 letras o mas es un nombre largo" $ do
      nombreLargo (UnaPersona 26 [packMedicina, queTodoSigaIgual] "Maixmiliano" 101 []) `shouldBe` True

  describe "Persona Suertuda" $ do
    it "Si el triple del coeficiente de satisfaccion de una persona es impar, la misma no es suertuda" $ do
      esPersonaSuertuda (UnaPersona 26 [packMedicina, queTodoSigaIgual] "Maixmiliano" 14 []) `shouldBe` False
    it "Si el triple del coeficiente de satisfaccion de una persona es par, la persona no es suertuda" $ do
      esPersonaSuertuda (UnaPersona 26 [packMedicina, queTodoSigaIgual] "Maixmiliano" 12 []) `shouldBe` True

  describe "Es un Nombre Lindo" $ do
    it "Una persona cuyo nombre no termina con a, no tiene nombre lindo" $ do
      nombreLindo (UnaPersona 30 [packMedicina, queTodoSigaIgual] "Ariel" 14 []) `shouldBe` False
    it "Una persona cuyo nombre termina con a, tiene nombre lindo" $ do
      nombreLindo (UnaPersona 21 [packMedicina, queTodoSigaIgual] "Melina" 12 []) `shouldBe` True

  describe "Se Recibio" $ do
    it "Una persona que se recibe debe recibir 1000 felicidonios por cada letra de la carrera y conseguir la habilidad" $ do
      recibirse "Ingenieria" (UnaPersona 21 [] "Juan" 100 []) `shouldBe` (UnaPersona 21 [] "Juan" 10100 ["Ingenieria"])
    --QUE TE PASAAAAAAAAAAAAAAAAAAAAA
    
  describe "Logro Viajar" $ do
    it "Una persona que viaja debe conseguir 100 felicidonios por cada ciudad que visita, y crecer un año" $ do
      viajar ["madrid", "barcelona"] pepe `shouldBe` (UnaPersona 24 [recibirse "medicina"] "Fransisca" 245 [])
  
  describe "Se enamoro" $ do
    it "Una persona enamorada debe sumar la cantidad de felicidonios de la persona de quien se enamoró" $ do
      enamorarse (UnaPersona 21 [] "Micaela" 100 []) pepe `shouldBe` (UnaPersona 23 [recibirse "medicina"] "Fransisca" 145 [])

  describe "Todo Sigue Igual" $ do
    it "Una persona que quiere que todo siga igual no debe cambiar" $ do
      queTodoSigaIgual pepe `shouldBe` pepe

  describe "Pack Medicina" $ do
    it "Una persona que consigue el pack medicinal debe conseguir los efectos de recibirse de medicina, de viajar a Berazategui y Paris y un bonus de 100 felicidonios" $ do
      packMedicina pepe `shouldBe` (UnaPersona 24 [recibirse "medicina"] "Fransisca" 8345 ["Medicina"])

  describe "Triplicar Efectos" $ do
    it "Una persona que quiere triplicar un sueño debe recibir los efectos del mismo pero triplicados" $ do
      triple (viajar ["El Chalten"]) pepe `shouldBe` (UnaPersona 26 [recibirse "medicina"] "Fransisca" 345 [])
    
    