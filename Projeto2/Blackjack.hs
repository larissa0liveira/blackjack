--fc59830, fc60807
module Blackjack
(   Baralho,
    converte,
    tamanho,
    EstadoJogo,
    inicializa,
    creditos,
    baralho,
    terminado,
    Estrategia(..),
    sempreStand,
    sempreHit,
    standSeGrande,
    simulaRonda,
    simulaJogo)
where

{- Cada string do baralho representa uma carta, em que o primeiro caracter é  
um valor {A,2,3,4,5,6,7,8,9,T,J,Q,K} e o segundo é um naipe {S,H,D,C} -}
type Baralho = [String]

converte :: [String] -> Baralho
converte xs = xs

--Devolve o numero de cartas de um baralho dado
tamanho :: Baralho -> Int
tamanho = length



{- Representa o estado de um jogo de Blackjack -}
data EstadoJogo = EstadoJogo {creditos::Int, maoJogador::Baralho, maoCasa::Baralho, baralho::Baralho}

{- Devolve o estado inicial do jogo com o baralho dado. No estado inicial 
o jogador tem 100 creditos e nenhuma carta foi distribuida -}
inicializa :: Baralho -> EstadoJogo
inicializa xs = EstadoJogo {creditos=100, baralho=xs, maoJogador=[], maoCasa=[]}

-- Verifica se o jogo terminou
terminado :: EstadoJogo -> Bool
terminado (EstadoJogo cred _ _ baralho) = cred <= 0 || tamanho baralho <= 20

{- A representação textual de EstadoJogo mostra as cartas do jogador,
as cartas da casa e o valor atual de créditos do jogador -}
instance Show EstadoJogo where
    show :: EstadoJogo -> String
    show (EstadoJogo cred jogador casa baralho) =
        "jogador: "++ toStringDeck jogador ++ "\n" ++
        "casa: "++ toStringDeck casa ++ "\n" ++
        "creditos: "++ show cred

{- Recebe um baralho e devolve uma string em que as cartas
são separadas apenas por um espaço -}
toStringDeck :: Baralho -> String
toStringDeck = foldr (\carta acc -> carta++" "++acc) []



{- Representa as funcoes que tomam decisoes pelo jogador. 
O jogador decide: 
> quanto apostar no inicio de uma ronda
> qual jogada (stand ou hit) deve fazer a cada momento
Existem 3 tipos de estrategia:
> Simples: o jogador faz sempre a mesma aposta e sempre a mesma jogada, independente do estado do jogo
> Media: o jogador faz sempre a mesma aposta, mas faz stand ou hit a depender do estado atual do jogo
> Avancada: o jogador decide a aposta e a jogada a cada momento, a depender do estado atual do jogo 

obs: Neste projeto nao foi implementada nenhuma estrategia do tipo Avancada
obs2: As decisoes do jogador nao devem utilizar o baralho do EstadoJogo -}
data Estrategia =   Simples {aposta::Int, jogada::String} | 
                    Media {aposta::Int, decideJogada::EstadoJogo -> String} |
                    Avancada {decideAposta::EstadoJogo -> Int, decideJogada::EstadoJogo -> String}

--Apostar sempre 5 créditos e fazer sempre stand
sempreStand :: Estrategia
sempreStand = Simples 5 "stand"

{- Apostar sempre 5 créditos e fazer hit sempre que possível.
O jogador só para se atingir 21 pontos -}
sempreHit :: Estrategia
sempreHit = Simples 5 "hit"

{- Apostar sempre 10 créditos e fazer hit enquanto sua pontuacao for inferior a 18,
o jogador para se tiver uma pontuacao igual ou superior a 18 -}
standSeGrande :: Estrategia
standSeGrande = Media 10 jogada
    where jogada (EstadoJogo _ maoJogador _ _ )
                | pontos maoJogador >= 18 = "stand"
                | otherwise = "hit"

instance Show Estrategia where
    show :: Estrategia -> String
    show (Simples aposta jogada) = "Estrategia simples: apostar sempre " ++ show aposta ++ " creditos, fazer sempre " ++ jogada
    show (Media aposta _) = "Estrategia intermediaria: apostar sempre " ++ show aposta ++ "creditos, fazer stand ou hit a depender do estado do jogo"
    show (Avancada _ _) = "Estrategia avancada: decisoes de aposta e jogada dependentes do estado do jogo"



{- Simula uma ronda do jogo Blackjack, utilizando a estrategia dada. 
Devolve o estado do jogo no final da ronda -}
simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
simulaRonda estrategia estado
    | terminado estado = estado
    | otherwise = jogaRonda estrategia $ EstadoJogo (apostar (mostraAposta estrategia estado) (creditos estado))    --faz aposta e distribui cartas
                                                    (maoJogador estado ++ take 2 deck)
                                                    (maoCasa estado ++ take 2 (drop 2 deck))
                                                    (drop 4 deck)
    where deck = baralho estado

--Devolve a decisao de aposta de acordo com a estrategia e estado de jogo dados
mostraAposta :: Estrategia -> EstadoJogo -> Int
mostraAposta (Avancada decideAposta _ ) estado = decideAposta estado
mostraAposta estrategia _ = aposta estrategia

--Devolve a decisao de jogada de acordo com a estrategia e estado de jogo dados
mostraJogada :: Estrategia -> EstadoJogo -> String
mostraJogada (Simples _ jogada) _ = jogada
mostraJogada estrategia estado = decideJogada estrategia estado

{- Verifica se uma aposta é valida. Devolve o numero de creditos atualizado se 
a aposta for valida e erro caso contrario -}
apostar :: Int -> Int -> Int
apostar aposta creditos
    | aposta > 0 && aposta <= creditos = creditos - aposta
    | otherwise = error "aposta invalida"

{- Realiza a jogada do jogador, de acordo com a estrategia dada -}
jogaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
jogaRonda estrategia estado
    | pontos (maoJogador estado) == 21  = compara (casaJoga estado) bet
    | pontos (maoJogador estado) < 21 = if mostraJogada estrategia estado == "stand" 
                                        then compara (casaJoga estado) bet
                                        else jogaRonda estrategia estado {maoJogador = maoJogador estado ++ take 1 (baralho estado), 
                                        baralho = tail (baralho estado)}
    | otherwise = estado {maoJogador = [], maoCasa = []}
    where bet = mostraAposta estrategia estado

-- Realiza a jogada da casa 
casaJoga :: EstadoJogo -> EstadoJogo
casaJoga estado
    | pontos (maoCasa estado) >= 17 = estado
    | otherwise = casaJoga estado { maoCasa = maoCasa estado ++ take 1 (baralho estado),
                                    baralho = tail (baralho estado) }

{- Faz a comparacao dos pontos do jogador e da casa, atualiza os creditos de acordo com o vencedor da ronda
e descarta as cartas das maos dos jogadores -}
compara :: EstadoJogo -> Int -> EstadoJogo
compara (EstadoJogo cred jogador casa deck) aposta = 
    EstadoJogo {creditos = atualizaCreditos jogador casa, maoJogador = [], maoCasa = [], baralho = deck}
    where   atualizaCreditos :: Baralho -> Baralho -> Int
            atualizaCreditos jogador casa
                | pontos casa < pontos jogador || pontos casa > 21 = cred + 2*aposta
                | pontos casa == pontos jogador =  cred + aposta
                | otherwise = cred


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


{- Dada uma estrategia do jogador e um baralho, corre uma simulacao de um jogo
completo de Blackjack, com um saldo inicial de 100 creditos.
Devolve o numero de creditos do jogador no final da simulacao -}
simulaJogo :: Estrategia -> Baralho -> Int
simulaJogo estrategia deck = simulaJogo' estrategia (inicializa (converte deck))

-- Funcao auxiliar para atualizar o estado do jogo a cada ronda
simulaJogo' :: Estrategia -> EstadoJogo -> Int
simulaJogo' estrategia estadoJogo
    | terminado estado = creditos estado
    | otherwise = simulaJogo' estrategia estado
        where estado = simulaRonda estrategia estadoJogo

