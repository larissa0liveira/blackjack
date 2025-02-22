
{- Modulo que contem funcoes e definicoes de tipos de dados sobre um jogo de Blackjack -}
module Blackjack 
(   Baralho,
    tamanho,
    geraBaralho,
    EstadoJogo(..),
    CartaValida(..),
    inicializa,
    terminado,
    apostar,
    compraCarta,
    casaJoga,
    compara,
    finaliza,
    pontos)
where

import Test.QuickCheck
import System.Random
import System.Random.Shuffle



{- Cada string do baralho representa uma carta, em que o primeiro caracter é  
um valor {A,2,3,4,5,6,7,8,9,T,J,Q,K} e o segundo é um naipe {S,H,D,C} -}
type Baralho = [String]

--Devolve o numero de cartas de um baralho dado
tamanho :: Baralho -> Int
tamanho = length

{- Dado um inteiro x e um gerador aleatorio, gera um baralho composto por x
baralhos normais (52 cartas) baralhado de forma aleatoria -}
geraBaralho :: RandomGen gen => Int -> gen -> Baralho   
geraBaralho x = shuffle' baralho (tamanho baralho)
  where baralho = concatMap (replicate x) baralhoSimples
        baralhoSimples = [[valor,naipe] | naipe <- "SHDC", valor <- "A23456789TJQK"]



-- Representa uma carta de baralho valida
newtype CartaValida = CV {carta::String}

instance Arbitrary CartaValida where
    arbitrary = do
        n <- choose(0,51) :: Gen Int
        return $ CV (baralhoSimples !! n)
        where baralhoSimples = [[valor,naipe] | naipe <- "SHDC", valor <- "A23456789TJQK"]

instance Show CartaValida where
    show (CV carta) = carta



{- Representa o estado de um jogo de Blackjack -}
data EstadoJogo = EstadoJogo {creditos::Int, maoJogador::Baralho, maoCasa::Baralho, baralho::Baralho, aposta::Int}

{- A representacao textual de EstadoJogo mostra as cartas do jogador e as cartas da casa -}
instance Show EstadoJogo where
    show :: EstadoJogo -> String
    show (EstadoJogo _ jogador casa baralho _) =
        "jogador: "++ toStringDeck jogador ++ "\n" ++
        "casa: "++ toStringDeck casa

{- Recebe um baralho e devolve uma string em que as cartas sao separadas apenas por um espaco -}
toStringDeck :: Baralho -> String
toStringDeck = foldr (\carta acc -> carta++" "++acc) []


instance Arbitrary EstadoJogo where
    arbitrary = oneof [estadoStand, estadoHit]

{- Gera um estado de jogo que representa o momento apos o jogador decidir a aposta e as cartas serem distribuidas,
ou seja, um estado de jogo quando o jogador faz "stand". A jogada que sucede esse estado é a jogada da casa -}
estadoStand :: Gen EstadoJogo
estadoStand = do
    cred <- choose(1,1000) :: Gen Int
    jogador <- vector 2 :: Gen [CartaValida]
    casa <- vector 2 :: Gen [CartaValida]
    b <- choose(17,500) :: Gen Int
    baralho <- vector b :: Gen [CartaValida]
    aposta <- choose(1,cred) :: Gen Int
    let maoJogador = map carta jogador
        maoCasa = map carta casa
        deck = map carta baralho
    return $ EstadoJogo cred maoJogador maoCasa deck aposta

{- Gera um estado de jogo que representa o momento imediatamente apos o jogador fazer "hit" (comprar uma ou mais cartas).
A jogada que sucede esse estado é a jogada da casa -}
estadoHit :: Gen EstadoJogo
estadoHit = do
    cred <- choose(1,1000) :: Gen Int
    j <- choose(3,10) :: Gen Int            --jogador ja fez um hit mas pode ter feito mais, maximo de 10 cartas foi uma escolha arbitraria
    jogador <- vector j :: Gen [CartaValida]
    casa <- vector 2 :: Gen [CartaValida]
    b <- choose(16,500) :: Gen Int
    baralho <- vector b :: Gen [CartaValida]
    aposta <- choose(1,cred) :: Gen Int
    let maoJogador = map carta jogador
        maoCasa = map carta casa
        deck = map carta baralho
    if pontos maoJogador > 21 
    then return $ EstadoJogo cred (validarMao maoJogador) maoCasa deck aposta
    else return $ EstadoJogo cred maoJogador maoCasa deck aposta

{- Funcao auxiliar que verifica se a mao aleatoria com mais de 21 pontos gerada para o jogador no EstadoHit é valida. 
Uma mao com mais de 21 pontos é valida se tiver ultrapassado os 21 pontos apenas com a ultima carta -}
validarMao :: Baralho -> Baralho
validarMao xs
    | pontos (init xs) < 21 = xs
    | otherwise = validarMao (init xs)



{- Devolve o estado inicial do jogo com o baralho dado. No estado inicial 
o jogador tem 100 creditos e nenhuma carta foi distribuida -}
inicializa :: Baralho -> EstadoJogo
inicializa xs = EstadoJogo {creditos=100, baralho=xs, maoJogador=[], maoCasa=[], aposta=0}

-- Verifica se o jogo terminou
terminado :: EstadoJogo -> Bool
terminado (EstadoJogo cred _ _ baralho _) = cred <= 0 || tamanho baralho <= 20

-- Realiza a aposta e distribui duas cartas para cada jogador
apostar :: Int -> EstadoJogo -> EstadoJogo
apostar aposta estado = EstadoJogo {creditos = creditos estado,
                                    maoJogador = take 2 (baralho estado),
                                    maoCasa = maoCasa estado ++ take 2 (drop 2 (baralho estado)),
                                    baralho = drop 4 (baralho estado),
                                    aposta = aposta}

-- Distribui uma carta do baralho para o jogador
compraCarta :: EstadoJogo -> EstadoJogo
compraCarta estado = estado {maoJogador = maoJogador estado ++ take 1 (baralho estado), 
                                          baralho = tail (baralho estado)}

-- Realiza a jogada da casa 
casaJoga :: EstadoJogo -> EstadoJogo
casaJoga estado
    | pontos (maoCasa estado) >= 17 = estado
    | otherwise = casaJoga estado { maoCasa = maoCasa estado ++ take 1 (baralho estado),
                                    baralho = tail (baralho estado) }

-- Faz a comparacao dos pontos do jogador e da casa e atualiza os creditos de acordo com o vencedor da ronda
compara :: EstadoJogo -> EstadoJogo
compara estado
    | pontos jogador <= 21 && (pontos casa < pontos jogador || pontos casa > 21) = estado {creditos = cred + bet}
    | pontos casa == pontos jogador =  estado
    | otherwise = estado {creditos = cred - bet}
    where jogador = maoJogador estado
          casa = maoCasa estado
          cred = creditos estado
          bet = aposta estado

--Descarta as cartas das maos dos jogadores e coloca o valor da aposta a 0 novamente
finaliza :: EstadoJogo -> EstadoJogo
finaliza estado = estado {maoJogador = [], maoCasa = [], aposta = 0}

{- Recebe uma mao e devolve a pontuacao total de acordo com as regras do Blackjack -}
pontos :: Baralho -> Int
pontos = pontos' 0
    where   pontos' :: Int -> Baralho -> Int
            pontos' soma [] = soma
            pontos' soma (x:xs)
                | head x == 'A' =   if pontos' (soma+11) xs > 21
                                    then pontos' (soma+1) xs
                                    else pontos' (soma+11) xs
                | otherwise = pontos' (soma + pontosCarta (head x)) xs

{- Funcao auxiliar que recebe o valor de uma carta e devolve o numero de pontos correspondente, 
excluindo o 'A' -}
pontosCarta :: Char -> Int
pontosCarta valor
    | valor `elem` "23456789" = read [valor]
    | otherwise = 10

