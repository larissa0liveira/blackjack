--Lista dos possiveis valores de uma carta
valores :: String
valores = "A23456789TJQK"

--Lista dos possiveis naipes de uma carta
naipes :: String
naipes = "SHDC"

baralho :: [String]
baralho = [[x,y] | y <- naipes, x <- valores]

--Numero de pontos referentes a cada carta do baralho no blackjack
pontos :: String -> Int -> Int
pontos carta soma
    | valor == 'A' && soma > 11 = 11
    | valor == 'A' = 1
    | elem valor "23456789" = read [valor]
    | otherwise = 10
    where valor = head carta

combinacoesBlackjack :: Int -> [(String, String)]
combinacoesBlackjack n = [(x,y) | x <- baralho, y <- baralho, x < y, pontos x n + pontos y (n-pontos x n) == n]

--Todas as combinacoes de trios possiveis com um baralho
trios :: [[String]]
trios = [[a,b,c] | a <- baralho, b <- baralho, c <- baralho, head a == head b, head b == head c, a < b, b < c]

--Todas as combinacoes de pares possiveis com um baralho
pares :: [[String]]
pares = [[d,e] | d <- baralho, e <- baralho, head d == head e, d < e]

fullHouses :: [[String]]
fullHouses = [c3 ++ c2 | c3 <- trios, c2 <- pares, head (head c3) /= head (head c2)]

