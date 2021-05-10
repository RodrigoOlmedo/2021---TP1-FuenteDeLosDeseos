module Spec where
import PdePreludat
import Library
import Test.Hspec

--Definimos una persona generica para simplificar los casos de prueba
pepe = UnaPersona{
    edad = 23
,   suenios = [Recibirse]
,   nombre = "Fransisca"
,   felicidonios = 45
,   habilidades = []
}

correrTests :: IO ()
correrTests = hspec $ do
  describe "Coeficiente de Satisfaccion" $ do
    it "El coeficiente de satisfacción de una persona muy feliz con 101 felicidonios y 25 años debe ser 2.525" $ do
      coeficienteDeSatisfaccion (UnaPersona 25 [] "Maria" 101 []) `shouldBe` 2525
    it "El coeficiente de satisfacción de una persona moderadamente feliz con 100 felicidonios y 2 sueños debe ser 200" $ do
      coeficienteDeSatisfaccion (UnaPersona 24 [Recibirse, Viajar] "Juan" 100 []) `shouldBe` 200
    it "El coeficiente de satisfacción de una persona poco feliz con 50 felicidonios debe ser 25" $ do
      coeficienteDeSatisfaccion (UnaPersona 50 [] "Pedro" 50 []) `shouldBe` 25

  describe "Grado de Ambicion" $ do
    it "El grado de ambición de una persona muy feliz con 101 felicidonios y 2 sueños debe ser 202" $ do
      gradoDeAmbicion (UnaPersona 26 [QuierePackMedicina, QueTodoSigaIgual] "Alf" 101 []) `shouldBe` 202
    it "El grado de ambición de una persona moderadamente feliz con 100 felicidonios, 26 años y 2 sueños debe ser 52" $ do
      gradoDeAmbicion (UnaPersona 26 [QuierePackMedicina, Viajar] "Alf" 100 []) `shouldBe` 52
    it "El grado de ambición de una persona poco feliz con 50 felicidonios y 1 sueño debe ser 2" $ do
      gradoDeAmbicion (UnaPersona 26 [Recibirse] "Alf" 50 []) `shouldBe` 2

  describe "Es un Nombre Largo" $ do
    it "El nombre Evangelina no es un nombre largo" $ do
      nombreLargo (UnaPersona 26 [QuierePackMedicina, QueTodoSigaIgual] "Evangelina" 101 []) `shouldBe` False
    it "El nombre Maximiliano es un nombre largo" $ do
      nombreLargo (UnaPersona 26 [QuierePackMedicina, QueTodoSigaIgual] "Maixmiliano" 101 []) `shouldBe` True

  describe "Persona Suertuda" $ do
    it "Una persona con 14 felicidonios no es suertuda" $ do
      personaSuertuda (UnaPersona 26 [QuierePackMedicina, QueTodoSigaIgual] "Maixmiliano" 14 []) `shouldBe` "No es suertuda"
    it "Una persona con 12 felicidonios es suertuda" $ do
      personaSuertuda (UnaPersona 26 [QuierePackMedicina, QueTodoSigaIgual] "Maixmiliano" 12 []) `shouldBe` "Es suertuda"

  describe "Es un Nombre Lindo" $ do
    it "Una persona Ariel no tiene nombre lindo" $ do
      nombreLindo (UnaPersona 30 [QuierePackMedicina, QueTodoSigaIgual] "Ariel" 14 []) `shouldBe` False
    it "Una persona Melina tiene nombre lindo" $ do
      nombreLindo (UnaPersona 21 [QuierePackMedicina, QueTodoSigaIgual] "Melina" 12 []) `shouldBe` True

  describe "Se Recibio" $ do
    it "Una persona que sueña recibirse y se recibe de ingenieria debe recibir 10000 feliciondios y conseguir la habilidad" $ do
      seRecibio "Ingenieria" (UnaPersona 21 [Recibirse] "Juan" 100 []) `shouldBe` (UnaPersona 21 [Recibirse] "Juan" 10100 ["Ingenieria"])
    it "Una persona que no sueña con recibirse y se recibe no debe cambiar" $ do
      seRecibio "Carrera" (UnaPersona 21 [] "Juan" 100 []) `shouldBe` (UnaPersona 21 [] "Juan" 100 [])
  
  describe "Logro Viajar" $ do
    it "Una persona que viaja a dos ciudades debe sumar 200 felicidonios y sumar un año" $ do
      viajo ["madrid", "barcelona"] pepe `shouldBe` (UnaPersona 24 [Recibirse] "Fransisca" 245 [])
  
  describe "Se enamoro" $ do
    it "Una persona que se enamoro de una persona con 100 felicidonios debe sumar esa misma cantidad" $ do
      seEnamoro (UnaPersona 21 [] "Micaela" 100 []) pepe `shouldBe` (UnaPersona 23 [Recibirse] "Fransisca" 145 [])

  describe "Todo Sigue Igual" $ do
    it "Una persona que quiere que todo siga igual no debe cambiar" $ do
      todoSigueIgual pepe `shouldBe` pepe

  describe "Pack Medicina" $ do
    it "Una persona que consigue el pack medicinal debe sumar 8300 felicidonios, sumar un año y conseguir la habilidad de Medicina" $ do
      packMedicina pepe `shouldBe` (UnaPersona 24 [Recibirse] "Fransisca" 8345 ["Medicina"])

  describe "Triplicar Efectos" $ do
    it "Una persona que quiere triplicar el sueño del pack medicinal debe sumar 24900 felicidonios, sumar tres años y conseguir la habilidad de Medicina" $ do
      triple QuierePackMedicina [] "" pepe pepe `shouldBe` (UnaPersona 26 [Recibirse] "Fransisca" 24945 ["Medicina"])
    it "Una persona que quiere triplicar el sueño de viajar a dos ciudades debe sumar 600 felicidonios y sumar tres años" $ do
      triple Viajar ["madrid", "Quilmes"] "" pepe pepe `shouldBe` (UnaPersona 26 [Recibirse] "Fransisca" 645 [])
    it "Una persona que quiere triplicar el sueño de recibirse de abogado debe sumar 21000 felicidonios y conseguir la habilidad de abogado" $ do
      triple Recibirse [] "abogado" pepe pepe `shouldBe` (UnaPersona 23 [Recibirse] "Fransisca" 21045 ["abogado"])
    it "Una persona que quiere triplicar el sueño de enamorarse de una persona con 45 felicidonios debe sumar 135 felicidonios" $ do
      triple Enamorarse [] " " pepe pepe `shouldBe` (UnaPersona 23 [Recibirse] "Fransisca" 180 [])
    it "Una persona que quiere triplicar el sueño de que todo siga igual no debe cambiar" $ do
      triple QueTodoSigaIgual [] " " pepe pepe `shouldBe` pepe