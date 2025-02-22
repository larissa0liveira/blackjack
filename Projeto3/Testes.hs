
{- Modulo de testes das funcoes implementadas no modulo Blackjack -}
module Testes (main) where

import System.Random
import Test.QuickCheck
import Test.QuickCheck.Random
import Data.List

import Blackjack(
    Baralho,
    tamanho,
    geraBaralho,
    EstadoJogo(..),
    CartaValida(..),
    terminado,
    compraCarta,
    casaJoga,
    compara,
    pontos)

main = do
    quickCheck prop_pontos_inicial 
    quickCheck prop_compara_creditos 
    quickCheck prop_casaJoga_pontos 
    quickCheck prop_compraCarta_baralho
    quickCheck prop_compara_derrota
    quickCheck prop_geraBaralho_elem

-- Uma mao inicial (de duas cartas) vale sempre no maximo 21 pontos
prop_pontos_inicial :: CartaValida -> CartaValida -> Bool
prop_pontos_inicial (CV c1) (CV c2) = let mao = [c1,c2] in pontos mao > 0 && pontos mao <= 21

{- O numero de creditos do jogador apos uma ronda é um de tres valores possiveis: n-a,n,n+a, 
onde n é o numero de creditos no inicio da ronda e a é o valor da aposta -}
prop_compara_creditos :: EstadoJogo -> Property
prop_compara_creditos estado = pontos (maoJogador estado) <= 21 ==> credFinal == n+a || credFinal == n || credFinal == n-a
    where n = creditos estado
          a = aposta estado
          credFinal = creditos $ compara $ casaJoga estado

-- No final da vez da casa, a mao da casa tem sempre pelo menos 17 pontos
prop_casaJoga_pontos :: EstadoJogo -> Bool
prop_casaJoga_pontos estado = pontos (maoCasa estadoFinal) >= 17
    where estadoFinal = casaJoga estado

{- A primeira carta do baralho é sempre a ultima carta da mao do jogador depois 
que ele faz hit (compra uma carta) -}
prop_compraCarta_baralho :: EstadoJogo -> Bool
prop_compraCarta_baralho estado = head (baralho estado) == last (maoJogador (compraCarta estado))

{- Sempre que o jogador decide fazer hit e termina com mais de 21 pontos,
os creditos apostados sao perdidos (derrota imediata, sem a casa jogar) -}
prop_compara_derrota :: EstadoJogo -> Property
prop_compara_derrota estado = isEstadoHit estado && pontos (maoJogador estado) > 21 ==> cred == n-a
    where n = creditos estado 
          a = aposta estado
          cred = creditos $ compara estado

-- Verifica se o estado gerado é um estadoHit
isEstadoHit :: EstadoJogo -> Bool
isEstadoHit estado = tamanho (maoJogador estado) > 2

{- Um baralho gerado aleatoriamente possui sempre X repeticoes de cada carta de um baralho normal,
em que X é o numero de baralhos normais dado pelo utilizador -}
prop_geraBaralho_elem :: Int -> QCGen -> Property
prop_geraBaralho_elem x gen = x > 0 ==> all (repeticoes x (geraBaralho x gen)) baralhoSimples
    where baralhoSimples = [[valor,naipe] | naipe <- "SHDC", valor <- "A23456789TJQK"]

-- Verifica se o numero de repeticoes de uma carta dada em um baralho dado é igual ao inteiro dado
repeticoes :: Int -> Baralho -> String -> Bool
repeticoes x aleatorio carta = length (elemIndices carta aleatorio) == x
          