import Test.HUnit
import TP1

-- TESTS

testsInvertido :: Test
testsInvertido = TestList -- TODO: AGREGAR
  [ "Caja invertida (1)"
    ~: invertido cajaOn
    ~?= cajaOn
  , "Caja invertida (2)"
    ~: invertido cajaOff
    ~?= cajaOff
  , "Caja invertida (3)"
    ~: invertido cajaNada
    ~?= cajaNada
  ]

testsHayCaminoIluminado :: Test
testsHayCaminoIluminado = TestList
  [ "Caja encendida tiene camino iluminado"
    ~: hayCaminoIluminado cajaOn
    ~?= True
  , "Caja apagada no tiene camino iluminado"
    ~: hayCaminoIluminado cajaOff
    ~?= False
  , "Caja Nada no tiene camino iluminado"
    ~: hayCaminoIluminado cajaNada
    ~?= False
  , "Serie de dos encendidas tiene camino iluminado"
    ~: hayCaminoIluminado (Serie cajaOn cajaOn)
    ~?= True
  , "Serie donde alguna esta apagada no tiene camino iluminado"
    ~: hayCaminoIluminado (Serie cajaOn cajaOff)
    ~?= False
  , "Paralelo con entrada/salida encendidas y al menos un ramal iluminado"
    ~: hayCaminoIluminado (Paralelo on cajaOn cajaOff on)
    ~?= True
  , "Paralelo con entrada apagada no tiene camino iluminado"
    ~: hayCaminoIluminado (Paralelo off cajaOn cajaOn on)
    ~?= False
  , "Paralelo con salida apagada no tiene camino iluminado"
    ~: hayCaminoIluminado (Paralelo on cajaOn cajaOn off)
    ~?= False
  , "Paralelo con ambos ramales apagados no tiene camino iluminado"
    ~: hayCaminoIluminado (Paralelo on cajaOff cajaOff on)
    ~?= False
  ]

testsCantidadPrendidas :: Test
testsCantidadPrendidas = TestList -- TODO: AGREGAR
  [ "Cantidad prendidas en caja prendida es 1"
    ~: cantidadPrendidas cajaOn
    ~?= 1
  ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito = TestList -- TODO: AGREGAR
  [ "La lista de cajas de un circuito con una única caja es la lista con esa caja"
    ~: cajasDeCircuito cajaOn
    ~?= [on]
  ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo = TestList -- TODO: AGREGAR
  [ "Una caja es prolija"
    ~: esCircuitoProlijo cajaOn
    ~?= True
  ]

-- NOTA: para correr este test, cambiar la línea 18 del archivo tp1.hs de "show = showDeCircuito" a
  -- "show = showDeCircuitoConEstructura".
  -- De esa forma, podrán distinguir la estructura de los circuitos en serie.
testsCircuitoEmprolijado :: Test
testsCircuitoEmprolijado = TestList -- TODO: AGREGAR
  [ "La versión emprolijada de una caja es la misma caja"
    ~: circuitoEmprolijado cajaOn
    ~?= cajaOn
  ]

testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura = TestList -- TODO: AGREGAR
  [
    
  ]

testsSubCircuitoMásResistente :: Test
testsSubCircuitoMásResistente = TestList -- TODO: AGREGAR
  [
    
  ]

tests :: Test
tests = TestList
  [ TestLabel "invertido"                testsInvertido
  , TestLabel "hayCaminoIluminado"       testsHayCaminoIluminado
  , TestLabel "cantidadPrendidas"        testsCantidadPrendidas
  , TestLabel "cajasDeCircuito"          testsCajasDeCircuito
  , TestLabel "esCircuitoProlijo"        testsEsCircuitoProlijo
  , TestLabel "circuitoEmprolijado"      testsCircuitoEmprolijado
  , TestLabel "tienenLaMismaEstructura"  testsTienenLaMismaEstructura
  , TestLabel "subCircuitoMásResistente" testsSubCircuitoMásResistente
  ]

main :: IO ()
main = runTestTT tests >>= print