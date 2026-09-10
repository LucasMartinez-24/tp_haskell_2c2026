module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- 1: recCircuito
recCircuito ::
    (Caja -> b) ->
    (Circuito -> Circuito -> b -> b -> b) ->
    (Circuito -> Circuito -> Caja -> b -> b -> Caja -> b) ->
    Circuito ->
    b
recCircuito cCaja cSerie cParalelo c =
    case c of
        Caja caja -> cCaja caja
        Serie circuitoInicial circuitoFinal -> cSerie circuitoInicial circuitoFinal (rec circuitoInicial) (rec circuitoFinal)
        Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida -> cParalelo circuitoDerecho circuitoIzquierdo cajaEntrada (rec circuitoIzquierdo) (rec circuitoDerecho) cajaSalida
    where
        rec = recCircuito cCaja cSerie cParalelo

-- 2: foldCircuito

foldCircuito ::
    (Caja -> b) ->
    (b -> b -> b) ->
    (Caja -> b -> b -> Caja -> b) ->
    Circuito ->
    b
foldCircuito cCaja cSerie cParalelo = recCircuito cCaja (const.const$cSerie) (const.const$cParalelo)

-- 3 invertido
invertido :: Circuito -> Circuito
invertido = foldCircuito Caja (flip$Serie) paraleloInvertido
    where
        paraleloInvertido cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida = Paralelo cajaSalida resultadoDerecho resultadoIzquierdo cajaEntrada

-- 4: hayCaminoIluminado

hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado = foldCircuito cCaja (&&) cParalelo
  where
    cCaja (Bombilla True) = True
    cCaja _               = False
    cParalelo cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida =
      cCaja cajaEntrada && (resultadoIzquierdo || resultadoDerecho) && cCaja cajaSalida

-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int
cantidadPrendidas = foldCircuito cCaja (+) cParalelo
  where
    cCaja (Bombilla True) = 1
    cCaja _               = 0
    cParalelo cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida =
      cCaja cajaEntrada + resultadoIzquierdo + resultadoDerecho + cCaja cajaSalida

-- 6: cajasDeCircuito

cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito = foldCircuito cCaja (++) cParalelo
  where
    cCaja c = [c]
    cParalelo cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida =
      [cajaEntrada] ++ resultadoIzquierdo ++ resultadoDerecho ++ [cajaSalida]

-- 7: esCircuitoProlijo

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo = recCircuito (const$True) cSerie cParalelo
  where
    cSerie _ (Serie _ _) _ _ = False
    cSerie _ _ resultadoInicial resultadoFinal = resultadoInicial && resultadoFinal
    cParalelo _ _ _ resultadoIzquierdo resultadoDerecho _ = resultadoIzquierdo && resultadoDerecho

-- 8: circuitoEmprolijado

circuitoEmprolijado :: Circuito -> Circuito
circuitoEmprolijado = foldCircuito cCaja cSerie cParalelo
  where
    cCaja caja = Caja caja
    cSerie resultadoInicial resultadoFinal = serieRotada resultadoInicial resultadoFinal
    cParalelo cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida =
      Paralelo cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida
    serieRotada circuitoInicial (Serie circuitoIzquierdo circuitoDerecho) =
      serieRotada (serieRotada circuitoInicial circuitoIzquierdo) circuitoDerecho
    serieRotada circuitoInicial circuitoFinal = Serie circuitoInicial circuitoFinal

-- 9: tienenLaMismaEstructura

tienenLaMismaEstructura = undefined -- TODO: COMPLETAR

-- 10: subCircuitoMásResistente

subCircuitoMásResistente = undefined -- TODO: COMPLETAR

{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

-- TODO: COMPLETAR

--}
