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








