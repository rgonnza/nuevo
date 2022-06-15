import Text.Show.Functions


-- -------------------- Funciones auxiliares y tipos --------------------
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

aumentarDanioArtefacto :: Int -> Heroe -> Heroe
aumentarDanioArtefacto cantidad heroe = heroe {
    artefacto = (nombreArtefacto (artefacto heroe) , cantidad + danio (artefacto heroe))
}

atacarAHeroe :: Villano -> Heroe -> Heroe
atacarAHeroe villano heroe 
    | sonAntagonistas villano heroe = heroe
    | otherwise = (arma villano) heroe

alterarNombreHeroe :: (String -> String) -> Heroe -> Heroe
alterarNombreHeroe modificador heroe = heroe {
    nombreHeroe = modificador . nombreHeroe $ heroe
}

sufijo :: String -> String -> String
sufijo cadena nombre = (++cadena) (nombre ++ " ")

prefijo :: String -> String -> String
prefijo = flip sufijo

type Artefacto = (String, Int)
nombreArtefacto (x,_) = x
danio (_,y) = y

type Arma = Heroe -> Heroe

data Heroe = Heroe {
    nombreHeroe :: String,
    vida :: Int,
    planetaHeroe :: String,
    artefacto :: Artefacto,
    enemigo :: Villano
} deriving Show

data Villano = Villano {
    nombreVillano :: String,
    planetaVillano :: String,
    arma :: Arma
} deriving Show

-- -------------------- Punto 1 --------------------
ironMan :: Heroe
ironMan = Heroe "Tony Stark" 100 "Tierra" ("Traje", 12) thanos

thor :: Heroe
thor = Heroe "Thor Odinson" 300 "Asgard" ("Stormbreaker", 0) loki

thanos :: Villano
thanos = Villano "Thanos" "Titan" guanteleteDelInfinito

loki :: Villano
loki = Villano "Loki Laufeyson" "Jotunheim" (cetro 20)

-- -------------------- Punto 2 --------------------
guanteleteDelInfinito :: Arma
guanteleteDelInfinito = alterarVidaHeroe ( disminuir 80)

cetro :: Int -> Arma
cetro porcentaje heroe
    | esTerricola heroe = romperArtefacto . alterarVidaHeroe (disminuirPorcentual 80) $ heroe
    | otherwise = alterarVidaHeroe (disminuirPorcentual 80) $ heroe

esTerricola :: Heroe -> Bool
esTerricola = (=="Tierra") . planetaHeroe 

romperArtefacto :: Heroe -> Heroe
romperArtefacto heroe = aumentarDanioArtefacto 30 heroe {
    artefacto = (nombreArtefacto (artefacto heroe) ++ " machacado", danio (artefacto heroe))
}

-- -------------------- Punto 3 --------------------
sonAntagonistas :: Villano -> Heroe -> Bool
sonAntagonistas (Villano unNombre planetaVillano _) (Heroe _ _ planetaHeroe _ villano)
    = (==planetaVillano) planetaHeroe || ((==unNombre) . nombreVillano $ villano) 
    
-- -------------------- Punto 4 --------------------
atacarEnGrupoAHeroe :: [Villano] -> Heroe -> Heroe
atacarEnGrupoAHeroe villanos heroe = foldl (flip atacarAHeroe) heroe villanos

-- -------------------- Punto 5 --------------------
supervivientes :: Villano -> [Heroe] -> [Heroe]
supervivientes villano = map (alterarNombreHeroe (prefijo "Super")) . (sobrevivenAlVillano villano)

sobrevivenAlVillano :: Villano -> [Heroe] -> [Heroe]
sobrevivenAlVillano villano = (filter sobrevive) . map (atacarAHeroe villano)

sobrevive :: Heroe -> Bool
sobrevive = (>= 50) . vida 


-- -------------------- Punto 6 --------------------
volverACasa :: [Heroe] -> [Heroe]
volverACasa =  descansar . (sobrevivenAlVillano thanos)

descansar :: [Heroe] -> [Heroe]
descansar  = map (arreglarArtefacto . alterarVidaHeroe (aumentar 30))

arreglarArtefacto :: Heroe -> Heroe
arreglarArtefacto heroe = heroe {
    artefacto = ( head . words  . nombreArtefacto $ artefacto heroe , 0)
}

-- -------------------- Punto 7 --------------------
villanoEsDebilAnteHeroes :: Villano -> [Heroe] -> Bool
villanoEsDebilAnteHeroes villano = all (\x -> sonAntagonistas villano x && tieneArtefactoMachacado x)

tieneArtefactoMachacado :: Heroe -> Bool
tieneArtefactoMachacado = (elem "machacado") . words . nombreArtefacto . artefacto