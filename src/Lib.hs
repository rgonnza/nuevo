import Text.Show.Functions




type Artefacto = (String, Int)
nombreArtefacto (x,_) = x
danioArtefacto (_,y) = y

type Arma = Heroe -> Heroe

data Heroe = Heroe {
    nombreHeroe :: String,
    vida :: Int,
    origenHeroe :: String,
    artefacto :: Artefacto,
    suVillano :: Villano
} deriving Show

data Villano = Villano {
    nombrevillano :: String,
    origenVillano :: String,
    arma :: Arma
} deriving Show

-- funciones sobre vida
alterarVidaHeroe :: (Int -> Int) -> Heroe -> Heroe
alterarVidaHeroe modificador heroe = heroe{
    vida = modificador . vida $ heroe
}

aumentar :: Int -> Int -> Int
aumentar cantidad = (+cantidad)

disminuir :: Int -> Int -> Int
disminuir cantidad = (subtract cantidad)

aumentarPorcentual :: Int -> Int -> Int
aumentarPorcentual porcentaje vida = (flip aumentar . flip div 100 . (*porcentaje) $ vida) vida 

disminuirPorcentual :: Int -> Int -> Int
disminuirPorcentual porcentaje vida = (flip disminuir . flip div 100 . (*porcentaje) $ vida) vida 

aumentarPorcentual' porcentaje vida = (`aumentar` aplicarPorcentaje porcentaje vida) vida
disminuirPorcentual' porcentaje vida = (`disminuir` aplicarPorcentaje porcentaje vida) vida

aplicarPorcentaje porcentaje cantidad = div (porcentaje * cantidad) 100



-- -------------------- Punto 1 --------------------

--1a
ironMan :: Heroe
ironMan = Heroe "Tony Stark" 100 "Tierra" ("Traje", 12) thanos

thor :: Heroe
thor = Heroe "Thor Odinson" 300 "Asgard" ("Stormbreaker", 0) loki

-- 1b
thanos :: Villano
thanos = Villano "Thanos" "Titan" guanteleteDelInfinito

loki :: Villano
loki = Villano "Loki Laufeyson" "Jotunheim" cetro

-- -------------------- Punto 2 --------------------

guanteleteDelInfinito :: Arma
guanteleteDelInfinito = alterarVidaHeroe (disminuirPorcentual' 80) 


cetro porcentaje heroe
    | esTerricola heroe = machacarArtefacto . aumentarDanioArtefacto . alterarVidaHeroe (disminuirPorcentual' porcentaje) $ heroe
    | otherwise = alterarVidaHeroe (disminuirPorcentual' porcentaje) heroe


aumentarDanioArtefacto cantidad heroe = heroe {
    artefacto = ( nombreArtefacto (artefacto heroe) , cantidad + danioArtefacto (artefacto heroe))
}
machacarArtefacto 

cambiarNombreArtefacto modificador heroe = heroe {

}