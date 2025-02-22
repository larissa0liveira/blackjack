{-
Este modulo é um programa para jogar Blackjack

> stack ghc Main.hs

A instrução acima produz um executável Main, que é executável
através de um dos seguintes quatro tipos de instruções:

> ./Main ficheiro -- carrega um baralho para jogar Blackjack
> ./Main          -- carrega o baralho default.bar
> ./Main -n X     -- carrega um baralho aleatório formado por X baralhos normais de cartas
> ./Main -t       -- corre os testes
-}

module Main (main) where

import System.Environment
import System.Directory
import System.Random
import System.Random.Shuffle

import qualified Testes

import Blackjack


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-n", x]  -> do
            gen <- newStdGen
            iniciarJogo (inicializa $ geraBaralho (read x) gen) 
        ["-t"] -> Testes.main
        [filename] -> do
            isFile <- doesFileExist filename
            if isFile 
            then do
                baralho <- readFile filename
                iniciarJogo (inicializa $ lines baralho)
            else do
                putStrLn "O ficheiro nao existe, tente novamente."
                utilizacao
        []          -> do
            baralho <- readFile "default.bar"
            iniciarJogo (inicializa $ lines baralho)
        _           -> utilizacao

-- Imprime a forma de utilizacao deste modulo
utilizacao :: IO ()
utilizacao = putStrLn $ "Utilizacao:\n./Main ficheiro -- carrega um baralho para jogar Blackjack\n" ++
                                    "./Main -- carrega o baralho default.bar\n" ++
                                    "./Main -n X -- carrega um baralho aleatório formado por X baralhos normais de cartas\n" ++
                                    "./Main -t -- corre os testes"

{- Imprime o numero de cartas no baralho e os creditos do jogador e verifica se o jogo esta terminado -}
iniciarJogo :: EstadoJogo -> IO ()
iniciarJogo estado = do
    putStrLn $ "cartas: " ++ show (tamanho (baralho estado)) ++ "\ncreditos: " ++ show (creditos estado)
    if terminado estado then sair estado else ronda estado

{- Inicia uma ronda com a decisao inicial do jogador. 
Caso o jogador decida apostar, verifica se a aposta é valida -}
ronda :: EstadoJogo -> IO ()
ronda estado = do 
    args <- getLine
    case words args of 
        ["apostar", n] ->   if read n > 0 && read n <= creditos estado
                            then do
                                let novoEstado = apostar (read n) estado
                                print novoEstado
                                vezJogador novoEstado
                            else do
                                putStrLn "Aposta invalida, tente novamente."
                                ronda estado 
        ["sair"] -> sair estado

{- Imprime o saldo final e termina o jogo -}
sair :: EstadoJogo -> IO()
sair estado = putStrLn $ "saldo final: " ++ show (creditos estado)

{- Realiza a jogada do jogador de acordo com a sua decisao de jogada (stand ou hit) -}
vezJogador :: EstadoJogo -> IO () 
vezJogador estado
    | pontos (maoJogador estado) == 21 = vezCasa estado 
    | otherwise = do
            args <- getLine
            case args of
                "stand" -> vezCasa estado
                "hit" -> hit estado 

{- Imprime o estado do jogo apos o jogador fazer hit e verifica a sua pontuacao -}
hit :: EstadoJogo -> IO ()
hit estado = do 
    let novoEstado = compraCarta estado
    print novoEstado
    if pontos (maoJogador novoEstado) > 21 
    then do
        putStrLn "Derrota"
        fimDeRonda (compara novoEstado)
    else vezJogador novoEstado
        
{- Realiza a jogada da casa e imprime o estado final do jogo e o resultado da ronda -}
vezCasa :: EstadoJogo -> IO ()
vezCasa estado = do
    let novoEstado = casaJoga estado
        estadoFinal = compara novoEstado
    print novoEstado
    printResultado (creditos novoEstado) (creditos estadoFinal)
    fimDeRonda estadoFinal

{- Imprime o resultado final da ronda para o jogador -}
printResultado :: Int -> Int -> IO ()
printResultado creditosInicio creditosFim
    | creditosFim > creditosInicio = putStrLn "Vitoria"
    | creditosFim == creditosInicio = putStrLn "Empate"
    | creditosFim < creditosInicio = putStrLn "Derrota"

{- Finaliza a ronda, isto é, descarta as maos do jogadores e imprime 
o tamanho atual do baralho e os creditos do jogador -}
fimDeRonda :: EstadoJogo -> IO ()
fimDeRonda estado = do
    let estadoFinal = finaliza estado
    iniciarJogo estadoFinal