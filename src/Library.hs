module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Gimnasta = Gimnasta {
    nombre :: String 
    , edad :: Number
    , peso :: Number
    , coeficienteTonificacion :: Number
 } deriving(Show)

pancho = Gimnasta "Francisco" 40.0 120.0 1.0
andres = Gimnasta "Andy" 22.0 80.0 6.0

type Ejercicio = Number -> Gimnasta -> Gimnasta
 
--Punto 1
estaSaludable :: Gimnasta -> Bool
estaSaludable gimnasta = (not (esObeso gimnasta)) && tonificacionApropiada gimnasta

esObeso :: Gimnasta -> Bool
esObeso = (>100) . peso

tonificacionApropiada :: Gimnasta -> Bool
tonificacionApropiada = (>5) . coeficienteTonificacion

--Punto 2
quemarCalorias :: Gimnasta -> Number -> Gimnasta
quemarCalorias gimnasta calorias = gimnasta { peso = (peso gimnasta) - (kilosAPerder gimnasta calorias) }

kilosAPerder :: Gimnasta -> Number -> Number
kilosAPerder gimnasta calorias   | esObeso gimnasta = calorias / 150 
                                 | bajaUnKiloSi gimnasta calorias =  1
                                 | otherwise =  calorias / (pesoPorEdad (peso gimnasta) gimnasta) 

bajaUnKiloSi :: Gimnasta -> Number -> Bool
bajaUnKiloSi gimnasta calorias = not (esObeso gimnasta) && (edad gimnasta) > 30 && calorias > 200 

pesoPorEdad :: Number -> Gimnasta -> Number 
pesoPorEdad peso = (*peso) . edad

--Punto 3A
caminataEnCinta :: Ejercicio
caminataEnCinta minutos gimnasta = quemarCalorias gimnasta (caloriasQuemadas 1 5 minutos ) 

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos gimnasta = quemarCalorias gimnasta (caloriasQuemadas 1 (velocidadPromedio minutos) minutos)

caloriasQuemadas :: Number -> Number -> Number -> Number
caloriasQuemadas caloria segun minutos = caloria * segun * minutos --CAMBIAR NOMBRE SEGUN

velocidadPromedio :: Number -> Number
velocidadPromedio = (/ 2) . (6 +) . velocidadMaxima 

velocidadMaxima :: Number -> Number
velocidadMaxima = (+6) . (/ 5)

--Punto 3B)
pesas :: Number -> Ejercicio
pesas kilos minutos = aumentarTonificacion (cuantoTonifica kilos minutos) 

cuantoTonifica :: Number -> Number -> Number
cuantoTonifica kilos minutos | minutos > 10 = 0.1 * kilos
                             | otherwise = 0

aumentarTonificacion :: Number -> Gimnasta -> Gimnasta
aumentarTonificacion tonificacion gimnasta = gimnasta { coeficienteTonificacion = (coeficienteTonificacion gimnasta) + tonificacion}

--Punto 3c)
colina :: Number -> Ejercicio
colina inclinacion minutos gimnasta =  quemarCalorias gimnasta (caloriasQuemadas 2 inclinacion minutos) 

--Punto 3d)
montania :: Number -> Ejercicio
montania inclinacion minutos =  (aumentarTonificacion 1) . (colina (inclinacion2daColina inclinacion) (duracion minutos)) . (colina inclinacion (duracion minutos))

duracion :: Number -> Number
duracion = (* 0.5)

inclinacion2daColina :: Number -> Number
inclinacion2daColina = (3+)

--Punto 4
--Dada una Rutina (es un Data con un nombre, duración total y lista de ejercicios específicos) y un gimnasta, 
--obtener al gimnasta luego de realizar la rutina. La cantidad de minutos dedicada a cada ejercicio es la misma. 
--Mostrar un ejemplo de uso usando todos los ejercicios del punto anterior. 
--Resolverlo usando recursividad.
--Hacer otra solución usando fold.

data Rutina = Rutina {
    nombreRutina :: String
    , duracionTotal :: Number 
    , listaDeEjercicios :: [Ejercicio]
}

--SOLUCION CON RESURSIVIDAD
{-
gimnastaLuegoDeHacerRutina :: Rutina -> Gimnasta -> Gimnasta
gimnastaLuegoDeHacerRutina rutina gimnasta= realizarRutinaConRecursividad (listaDeEjercicios rutina) gimnasta (duracionDeCadaEjercicio rutina)

realizarRutinaConRecursividad :: [Ejercicio] -> Gimnasta -> Number -> Gimnasta
realizarRutinaConRecursividad [] gimnasta minutos= gimnasta
realizarRutinaConRecursividad (x : xs) gimnasta minutos =  realizarRutinaConRecursividad xs (x minutos gimnasta) minutos

duracionDeCadaEjercicio :: Rutina -> Number
duracionDeCadaEjercicio rutina = (((duracionTotal rutina)  / ) . length . listaDeEjercicios) rutina
-}

--SOLUCION CON FOLDL
gimnastaLuegoDeHacerRutina :: Rutina -> Gimnasta -> Gimnasta
gimnastaLuegoDeHacerRutina rutina gimnasta= realizarRutinaConFoldl (listaDeEjercicios rutina) gimnasta (duracionDeCadaEjercicio rutina)

realizarRutinaConFoldl:: [Ejercicio] -> Gimnasta -> Number -> Gimnasta
realizarRutinaConFoldl rutina gimnasta minutos = foldl (realizarEjercicio minutos) gimnasta rutina

realizarEjercicio :: Number -> Gimnasta -> Ejercicio -> Gimnasta
realizarEjercicio minutos gimnasta ejercicio = ejercicio minutos gimnasta 

duracionDeCadaEjercicio :: Rutina -> Number
duracionDeCadaEjercicio rutina = (((duracionTotal rutina)  / ) . length . listaDeEjercicios) rutina

--EJEMPLO DE USO:
rutina1 = Rutina "Rutina Fast" 50 [caminataEnCinta , entrenamientoEnCinta, (pesas 2), (colina 5), (montania 10)] 
{-
Si a andres que originalmente tiene una estructura de: andres = Gimnasta "Andy" 22.0 80.0 6.0 lo hacemos realizar una rutina
formada por la lista de Ejercicios: [caminataEnCinta , entrenamientoEnCinta, (pesas 2), (colina 5), (montania 10)] , la cual tiene una duracion
total de 50 minutos (10 minutos cada ejercicio) entonces, luego de realizarla, quedaria de la siguiente forma:
<gimnastaLuegoDeHacerRutina rutina1 andres
Gimnasta {nombre = "Andy", edad = 22, peso = 79.74399822, coeficienteTonificacion = 7} 
-}

--Punto 4B) 
resumenRutina :: Rutina -> Gimnasta -> (NombreRutina, KilosPerdidos, TonificacionGanada)
resumenRutina rutina gimnasta = (nombreRutina rutina, cambiosObtenidosEn rutina peso gimnasta, cambiosObtenidosEn rutina coeficienteTonificacion gimnasta)

type Criterio = Gimnasta -> Number

cambiosObtenidosEn :: Rutina -> Criterio -> Gimnasta -> Number
cambiosObtenidosEn  rutina criterio gimnasta = (criterio gimnasta) - ((criterio . (gimnastaLuegoDeHacerRutina rutina)) gimnasta)

--Punto 5
--Dada una lista de rutinas, obtener un resumen de todas las que (individualmente) pueden llevar 
--a un gimnasta dado a estar saludable. --FALTA EL MAP !!!!!!!

type KilosPerdidos = Number
type TonificacionGanada =Number
type NombreRutina = String 
type Resumen = (NombreRutina, KilosPerdidos, TonificacionGanada)

obtenerResumenes :: [Rutina] -> Gimnasta -> [Resumen]
obtenerResumenes listaRutinas gimnasta = map (flip resumenRutina gimnasta) (rutinasSaludables listaRutinas gimnasta)

rutinasSaludables :: [Rutina] -> Gimnasta -> [Rutina] 
rutinasSaludables rutinas gimnasta = filter (esRutinaSaludable gimnasta) rutinas

esRutinaSaludable :: Gimnasta -> Rutina -> Bool
esRutinaSaludable gimnasta = estaSaludable . (flip gimnastaLuegoDeHacerRutina gimnasta) 