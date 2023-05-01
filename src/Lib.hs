import Data.list

data Grupo = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

data Elemento = Elemento {
    nombre::String,
    simboloQ :: String,
    nAtomico :: Int,
    grupoE :: Grupo
} 

data Compuesto = Compuesto {
    componente :: [(Sustancia , Int)],
    grupoC :: Grupo
} deriving (Show,Eq)

data Sustancia = Elem Elemento | Comp Compuesto deriving (Show,Eq)

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






