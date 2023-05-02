import Data.List

data Grupo = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

data Elemento = Elemento {
    nombre::String,
    simboloQuimico :: String,
    numeroAtomico :: Int,
    grupoElemento :: Grupo
} deriving (Show, Eq)

data Compuesto = Compuesto {
    componente :: [(Sustancia , Int)],
    grupoCompuesto :: Grupo
} deriving (Show,Eq)

data Sustancia = Elementos Elemento | Compuestos Compuesto deriving (Show, Eq)

-- ********** Ejercicio 1 ********** -- 
hidrogeno :: Sustancia
hidrogeno = Elementos (Elemento{
                nombre = "hidrogeno",
                simboloQuimico = "H",
                numeroAtomico = 1,
                grupoElemento = NoMetal
            })

oxigeno :: Sustancia
oxigeno = Elementos(Elemento{
                nombre = "oxigeno",
                simboloQuimico = "O",
                numeroAtomico = 8,
                grupoElemento = NoMetal
            })

cloro :: Sustancia
cloro = Elementos(Elemento{
                nombre = "cloro",
                simboloQuimico = "Cl",
                numeroAtomico = 15,
                grupoElemento = NoMetal
            })

sodio :: Sustancia
sodio = Elementos(Elemento{
                nombre = "sodio",
                simboloQuimico = "Na",
                numeroAtomico = 11,
                grupoElemento = NoMetal
            })

agua :: Sustancia
agua = Compuestos(Compuesto{
            componente = [(hidrogeno,2),(oxigeno,1)],
            grupoCompuesto = NoMetal
        })
-- ********** Ejercicio 1 ********** -- 

-- ********** Ejercicio 2 ********** -- 
conduceBien :: Sustancia -> String -> Bool
conduceBien (Elementos elemento) criterio
    | grupoElemento elemento == Metal = True -- Los metales conducen bien cualquier criterio
    | grupoElemento elemento == GasNoble && criterio == "electricidad" = True -- Los gases nobles conducen bien la electricidad
    | grupoElemento elemento == Halogeno && criterio == "calor" = True -- Los compuestos halógenos conducen bien el calor
    | otherwise = False -- Para el resto, no son buenos conductores
conduceBien (Compuestos compuesto) criterio
    | grupoCompuesto compuesto == Metal = True -- Los metales conducen bien cualquier criterio
    | grupoCompuesto compuesto == GasNoble && criterio == "electricidad" = True -- Los gases nobles conducen bien la electricidad
    | grupoCompuesto compuesto == Halogeno && criterio == "calor" = True -- Los compuestos halógenos conducen bien el calor
    | otherwise = False -- Para el resto, no son buenos conductores
-- ********** Ejercicio 2 ********** -- 

-- ********** Ejercicio 3 ********** --
primeraSustancia :: Sustancia -> Sustancia
primeraSustancia (Compuestos compuesto) = fst (head (componente compuesto))

nombreSustancia :: Sustancia -> String
nombreSustancia (Elementos elemento) = nombre elemento

palabra :: Sustancia -> String
palabra = (nombreSustancia . primeraSustancia)

ultimaLetra :: String -> Char
ultimaLetra palabra = last palabra

esVocal :: Char -> Bool
esVocal caracter = caracter `elem` "aeiouAEIOU"

terminaEnVocal :: Sustancia -> Bool
terminaEnVocal = (esVocal . (ultimaLetra . palabra))

posicionUltimaConsonante :: String -> Int
posicionUltimaConsonante nombres
                                | ((esVocal . ultimaLetra) (take ((length nombres) - 1) nombres)) == False = (length nombres - 1)
                                | ((esVocal . ultimaLetra) (take ((length nombres) - 2) nombres)) == False = (length nombres - 2)
                                | ((esVocal . ultimaLetra) (take ((length nombres) - 3) nombres)) == False = (length nombres - 3)
                                | otherwise = (length nombres) - 4

concatenarSiVocal :: String -> Int -> String
concatenarSiVocal palabra posicionConsonante = take posicionConsonante palabra

terminaVocalElemento :: Sustancia -> Bool
terminaVocalElemento = (esVocal . (ultimaLetra . nombreSustancia))

nombreUnion :: Sustancia -> String 
nombreUnion (Compuestos compuesto)
                    | (terminaEnVocal (Compuestos compuesto)) == True = (concatenarSiVocal (palabra (Compuestos compuesto)) (posicionUltimaConsonante (palabra (Compuestos compuesto)))) ++ "uro"
                    | otherwise = (palabra (Compuestos compuesto)) ++ "uro"
nombreUnion (Elementos elemento)
                    | (terminaVocalElemento (Elementos elemento)) == True = (concatenarSiVocal (nombreSustancia (Elementos elemento)) (posicionUltimaConsonante (nombreSustancia (Elementos elemento)))) ++ "uro"
                    | otherwise = (show elemento) ++ "uro"
-- ********** Ejercicio 3 ********** --

-- ********** Ejercicio 4 ********** --
segundaSustancia :: Sustancia -> Sustancia
segundaSustancia (Compuestos compuesto) = fst ((componente compuesto) !! 1)
combinar :: Sustancia -> Sustancia-> String
combinar sustanciaUno sustanciaDos = ((nombreUnion sustanciaUno) ++ " de " ++ (nombreSustancia sustanciaDos))
-- ********** Ejercicio 4 ********** --

-- ********** Ejercicio 5 ********** --
nuevoCompuesto :: Sustancia -> Sustancia -> Sustancia
nuevoCompuesto (Elementos elemento1) (Elementos elemento2) = Compuestos (Compuesto{
    componente = [((Elementos elemento1),1),((Elementos elemento2),1)],
    grupoCompuesto = NoMetal
})

mezclar :: Sustancia -> Sustancia -> String
mezclar elemento1 elemento2 = (combinar elemento1 elemento2) ++ " = " ++ (show (nuevoCompuesto elemento1 elemento2))
-- ********** Ejercicio 5 ********** --

-- ********** Ejercicio 6 ********** --
formulaElemento :: Sustancia -> String
formulaElemento (Elementos elemento) = simboloQuimico elemento

formulaPrimeraSustancia :: Sustancia -> String
formulaPrimeraSustancia = (formulaElemento . primeraSustancia)

formulaSegundaSustancia :: Sustancia -> String
formulaSegundaSustancia = (formulaElemento . segundaSustancia)

cantidadMoleculasPrimeraSustancia :: Sustancia -> Int
cantidadMoleculasPrimeraSustancia (Compuestos compuesto) = snd ((componente compuesto) !! 0)

cantidadMoleculasSegundaSustancia :: Sustancia -> Int
cantidadMoleculasSegundaSustancia (Compuestos compuesto) = snd ((componente compuesto) !! 1)

esMayorAUno :: Int -> String
esMayorAUno numero
                | numero > 1 = show (numero)
                | numero <= 1 = ""
                | otherwise = "Se rompio todo"

formula :: Sustancia -> String
formula (Compuestos compuesto) = (formulaPrimeraSustancia (Compuestos compuesto)) ++ ((esMayorAUno . cantidadMoleculasPrimeraSustancia) (Compuestos compuesto)) ++ (formulaSegundaSustancia (Compuestos compuesto)) ++ ((esMayorAUno . cantidadMoleculasSegundaSustancia) (Compuestos compuesto))
formula (Elementos elemento) = formulaElemento (Elementos elemento)
-- ********** Ejercicio 6 ********** --