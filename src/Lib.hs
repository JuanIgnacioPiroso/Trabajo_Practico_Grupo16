data Grupo = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

data Elemento = Elemento {
    nombre::String,
    simboloQ :: String,
    nAtomico :: Int,
    grupoE :: Grupo
} 









