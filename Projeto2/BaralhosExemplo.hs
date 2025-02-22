module BaralhosExemplo where

baralhoOrdenado :: [String] -- sempreStand 20, sempreHit 0
baralhoOrdenado = [ [valor,naipe] | naipe <- "SHDC", valor <- "A23456789TJQK", _ <- [1..6] ]

baralhoInsuficiente :: [String] -- sempreStand 100, sempreHit 100
baralhoInsuficiente = replicate 20 "QC"

baralhoSimples :: [String] -- sempreStand 105, sempreHit 95
baralhoSimples = ["8S", "2D", "6S", "6H"] ++ baralhoInsuficiente

baralhoEmpate :: [String] -- sempreStand 100, sempreHit 100
baralhoEmpate = ["TS", "AD", "2S", "5S", "4C", "KC"] ++ baralhoInsuficiente

baralhoPerdido :: [String] -- sempreStand 95, sempreHit 90
baralhoPerdido = ["TS", "7D", "5S", "AS", "TC", "2H"] ++ baralhoInsuficiente

baralhoGanho :: [String] -- sempreStand 110, sempreHit 105
baralhoGanho = ["TS", "4D", "JS", "5S", "7S", "AC"] ++ baralhoInsuficiente