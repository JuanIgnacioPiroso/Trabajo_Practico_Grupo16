import Data.List

data Grupo = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

data Elemento = Elemento {
    nombre::String,
    simboloQ :: String,
    nAtomico :: Int,
    grupoE :: Grupo
} deriving (Show, Eq)

data Compuesto = Compuesto {
    componente :: [(Sustancia , Int)],
    grupoC :: Grupo
} deriving (Show,Eq)

data Sustancia = Elem Elemento | Comp Compuesto deriving (Show, Eq)

-- ********** Ejercicio 1 ********** -- 
hidrogeno :: Sustancia
hidrogeno = Elem (Elemento{
                nombre = "hidrogeno",
                simboloQ = "H",
                nAtomico = 1,
                grupoE = NoMetal
            })

oxigeno :: Sustancia
oxigeno = Elem(Elemento{
                nombre = "oxigeno",
                simboloQ = "O",
                nAtomico = 8,
                grupoE = NoMetal
            })

agua :: Sustancia
agua = Comp(Compuesto{
            componente = [(hidrogeno,2),(oxigeno,1)],
            grupoC = NoMetal
        })
-- ********** Ejercicio 1 ********** -- 

-- ********** Ejercicio 2 ********** -- 
conduceBien :: Sustancia -> String -> Bool
conduceBien (Elem elemento) criterio
    | grupoE elemento == Metal = True -- Los metales conducen bien cualquier criterio
    | grupoE elemento == GasNoble && criterio == "electricidad" = True -- Los gases nobles conducen bien la electricidad
    | grupoE elemento == Halogeno && criterio == "calor" = True -- Los compuestos halógenos conducen bien el calor
    | otherwise = False -- Para el resto, no son buenos conductores
conduceBien (Comp compuesto) criterio
    | grupoC compuesto == Metal = True -- Los metales conducen bien cualquier criterio
    | grupoC compuesto == GasNoble && criterio == "electricidad" = True -- Los gases nobles conducen bien la electricidad
    | grupoC compuesto == Halogeno && criterio == "calor" = True -- Los compuestos halógenos conducen bien el calor
    | otherwise = False -- Para el resto, no son buenos conductores
-- ********** Ejercicio 2 ********** -- 

-- ********** Ejercicio 3 ********** --
obtenerSustancia :: Sustancia -> Sustancia
obtenerSustancia (Comp compuesto) = fst (head (componente compuesto))

obtenerNombreSustancia :: Sustancia -> String
obtenerNombreSustancia (Elem elemento) = nombre elemento

obtenerPalabra :: Sustancia -> String
obtenerPalabra = (obtenerNombreSustancia . obtenerSustancia)

obtenerUltimaLetra :: String -> Char
obtenerUltimaLetra palabra = last palabra

esVocal :: Char -> Bool
esVocal caracter = caracter `elem` "aeiouAEIOU"

terminaEnVocal :: String -> Bool
terminaEnVocal = (esVocal . obtenerUltimaLetra)


-- ********** Ejercicio 3 ********** --


