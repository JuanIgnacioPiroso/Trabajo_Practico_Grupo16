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

nombreDeUnion :: Sustancia -> String 
nombreDeUnion compuesto
                    | (terminaEnVocal compuesto) == True = (concatenarSiVocal (palabra compuesto) (posicionUltimaConsonante (palabra compuesto))) ++ "uro"
                    | otherwise = (palabra compuesto) ++ "uro"
-- ********** Ejercicio 3 ********** --

-- ********** Ejercicio 4 ********** --
-- combinar :: String -> String -> String
-- combinar sustanciaUno sustanciaDos = (concatenar sustanciaUno) ++ " de " ++ (sustanciaDos) 
-- ********** Ejercicio 4 ********** --

-- ********** Ejercicio 5 ********** --

-- ********** Ejercicio 5 ********** --

-- ********** Ejercicio 6 ********** --

segundaSustancia :: Sustancia -> Sustancia
segundaSustancia (Compuestos compuesto) = fst ((componente compuesto) !! 1)

formulaElemento :: Sustancia -> String
formulaElemento (Elementos elemento) = simboloQuimico elemento

cantidadMoleculasPrimeraSustancia :: Sustancia -> String
cantidadMoleculasPrimeraSustancia (Compuestos compuesto) = show (snd ((componente compuesto) !! 0))

cantidadMoleculasSegundaSustancia :: Sustancia -> String
cantidadMoleculasSegundaSustancia (Compuestos compuesto) = show (snd ((componente compuesto) !! 1))

--formulaCompuesto :: Sustancia -> String
--formulaCompuesto (Compuestos compuesto) = ((formulaElemento . primeraSustancia) $ compuesto) ++ (cantidadMoleculasPrimeraSustancia $ compuesto)


--formula :: Sustancia 
                                                
-- ********** Ejercicio 6 ********** --