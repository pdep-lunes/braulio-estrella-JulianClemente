module Spec where
import PdePreludat
import Library
import Test.Hspec
data Personaje = UnPersonaje {nombre :: String, poderbasico :: string, superpoder :: string, poderactivo :: Bool, vida :: Float, aliado :: Bool, sanador :: Bool, radio :: Int}

ataquebasico :: Personaje -> Personaje
ataquebasico atacante defensor
        |atacante {poderbasico} == "bolaEspinosa" = bolaEspinosa receptor
        |atacante {poderbasico}, == "lluviaDeTuercas" = lluviadeTuercas sanador atacante receptor
        |atacante {poderbasico} == "granadaDeEspinas" = granadaDeEspinas radio atacante receptor
        | atacante {poderbasico} == "torretaCurativa" = torretaCurativa receptor
ataqueespecial :: Personaje -> Personaje
ataqueespecial atacante receptor
        |atacante {superpoder} == "bolaEspinosa" = bolaEspinosa receptor
        |atacante {superpoder}, == "lluviaDeTuercas" = lluviadeTuercas sanador atacante receptor
        |atacante {superpoder} == "granadaDeEspinas" = granadaDeEspinas radio atacante receptor
        | atacante {superpoder} == "torretaCurativa" = torretaCurativa receptor
dañar :: Personaje -> Int -> Personaje
dañar dañado daño
      |dañado vida > daño = dañado {vida - daño}
      |otherwise dañado {vida = 0}
bolaEspinosa :: Personaje -> Personaje
bolaEspinosa receptor = dañar receptor 1000
lluviadeTuercas :: Bool -> Personaje -> Personaje
lluviadeTuercas sanador receptor
      |sanador && receptor aliado = receptor {vida + 800}
      |not.sanador && receptor not.aliado = receptor {vida/2}
      |otherwise receptor = receptor
granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio receptor
      | radio > 3 = receptor  {nombre + "Espina estuvo aqui"}
      | radio > 3 && receptor vida < 800 = receptor {poderactivo = False, vida = 0}
      | otherwise bolaEspinosa receptor
 torretaCurativa :: Personaje -> Personaje
torretaCurativa receptor
      | receptor aliado = receptor {poderactivo = True, vida * 2}
      | otherwise receptor = receptor
  
  atacaconpoderespecial :: Personaje -> Bool
  atacaconpoderespecial atacante 
      | atacante poderactivo = print(atacante " ataca con poder especial y con basico")
      | otherwise atacante = atacante
  estanenultimas :: [Personaje] -> [Personaje]
  estanenultimas personajesenpartida = filter (map personajesenpartida vida < 800)
  Espina :: Personaje
  Espina {nombre = "Espina", poderbasico = "bolaEspina", superpoder = "granadaDeEspinas"}



correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2



