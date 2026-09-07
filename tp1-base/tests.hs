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
testsCantidadPrendidas = TestList
  [ "Cantidad prendidas en caja prendida es 1"
    ~: cantidadPrendidas cajaOn
    ~?= 1
  , "Cantidad prendidas en caja apagada es 0"
    ~: cantidadPrendidas cajaOff
    ~?= 0
  , "Cantidad prendidas en caja Nada es 0"
    ~: cantidadPrendidas cajaNada
    ~?= 0
  , "Cantidad prendidas en serie de dos encendidas es 2"
    ~: cantidadPrendidas (Serie cajaOn cajaOn)
    ~?= 2
  , "Cantidad prendidas en serie con una apagada es 1"
    ~: cantidadPrendidas (Serie cajaOn cajaOff)
    ~?= 1
  , "Cantidad prendidas en paralelo cuenta entrada, ramales y salida"
    ~: cantidadPrendidas (Paralelo on cajaOn cajaOff on)
    ~?= 3
  ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito = TestList
  [ "La lista de cajas de un circuito con una única caja encendida es la lista con esa caja"
    ~: cajasDeCircuito cajaOn
    ~?= [on]
  , "La lista de cajas de un circuito con una única caja apagada es la lista con esa caja"
    ~: cajasDeCircuito cajaOff
    ~?= [off]
  , "La lista de cajas de un circuito con una única caja vacía es la lista con esa caja"
    ~: cajasDeCircuito cajaNada
    ~?= [Nada]
  , "En una serie, las cajas aparecen en orden: primero las del circuito inicial, luego las del final"
    ~: cajasDeCircuito (Serie cajaOn cajaOff)
    ~?= [on, off]
  , "En un paralelo, las cajas aparecen en orden: entrada, izquierda, derecha, salida"
    ~: cajasDeCircuito (Paralelo on cajaOn cajaOff off)
    ~?= [on, on, off, off]
  , "Circuito de ejemplo del enunciado"
    ~: cajasDeCircuito
         ( Serie
             ( Paralelo
                 on
                 (Paralelo off cajaNada cajaOn on)
                 (Paralelo Nada cajaOn cajaOff Nada)
                 on
             )
             cajaOn
         )
    ~?= [on, off, Nada, on, on, Nada, on, off, Nada, on, on]
  ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo = TestList
  [ "Una caja es prolija"
    ~: esCircuitoProlijo cajaOn
    ~?= True
  , "Una serie que asocia a la izquierda es prolija (ejemplo del enunciado)"
    ~: esCircuitoProlijo (Serie (Serie cajaOn cajaOff) cajaOn)
    ~?= True
  , "Una serie que asocia a la derecha no es prolija (ejemplo del enunciado)"
    ~: esCircuitoProlijo (Serie cajaOn (Serie cajaOff cajaOn))
    ~?= False
  , "Un paralelo con ambos subcircuitos prolijos es prolijo"
    ~: esCircuitoProlijo (Paralelo on cajaOn cajaOff off)
    ~?= True
  , "Un paralelo con un subcircuito no prolijo no es prolijo"
    ~: esCircuitoProlijo (Paralelo on (Serie cajaOn (Serie cajaOff cajaOn)) cajaOff off)
    ~?= False
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