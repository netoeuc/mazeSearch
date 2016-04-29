import Data.List
import System.Random

-- Euclides Carlos Pinto Neto
-- Julho de 2015
-- Disciplina: Paradigmas de programação
-- Professor: Lucas Albertins

-- A matriz que representa o labirinto segue o seguinte padrão:
-- 		0 -> Bloco branco, espaço vazio, se pode caminhar.
-- 		1 -> Bloco preto, parede, não se pode caminhar.
-- 		2 -> Lugar ocupado pelo herói.
--      4 -> Lugares visitados que não fazem parte do caminho principal (solução)
--      5 -> Caminho principla (solução)

--             >>>>   Funções relacionadas à construção do labirinto <<<<          

-- Esta função gera a matriz a partir de um número inteiro. O número de colunas é igual ao número de linhas
gerarMatriz:: Int -> [[Int]]
gerarMatriz n = [[1|x<-[1..n]] | x<-[1..n]]

--Função que retorna a quantidade de casas brancas no labirinto
qBranco:: Int -> Int -> [[Int]] -> Int
qBranco linha coluna matriz
	|linha==length(matriz) = 0
	|coluna==length(matriz) = qBranco (linha+1) 0 matriz
	|(matriz!!linha!!coluna==0) = 1 + qBranco linha (coluna+1) matriz
	|otherwise = qBranco linha (coluna+1) matriz

--getBrancoIndice:: Int -> Int -> Int -> [[Int]] -> [Int]
getBrancoIndice n linha coluna matriz
	|n==0 = [linha,(coluna-1)]
	|coluna==length(matriz) = getBrancoIndice n (linha+1) 0 matriz
	|linha==length(matriz) = [(linha-1),coluna]
	|matriz!!linha!!coluna==0 = getBrancoIndice (n-1) linha (coluna+1) matriz
	|otherwise = getBrancoIndice (n) linha (coluna+1) matriz

--Função que retorna a quantidade de casas pretas no labirinto
qPreto:: Int -> Int -> [[Int]] -> Int
qPreto linha coluna matriz
	|linha==length(matriz) = 0
	|coluna==length(matriz) = qPreto (linha+1) 0 matriz
	|(matriz!!linha!!coluna==1) = 1 + qPreto linha (coluna+1) matriz
	|otherwise = qPreto linha (coluna+1) matriz

--fromIntegral
--Função que retorna a procentagem de quadrados brancos
porcentagemBranco::[[Int]]->Float
porcentagemBranco matriz = fromIntegral(qBranco 0 0 matriz)*100/(fromIntegral(length(matriz))^2)

-- Apenas replicando a função lenght
getTamanho:: [[Int]] -> Int
getTamanho m = length(m)

-- Defaul = 0 0 matriz. 
-- Problemas com números randômicos gerar labirinto (tipo IO Int ao invés de Int)
-- Esta função altera o valor da matriz através dos índices passados (linha e coluna). O valor é alterado de 1 para 0. As funções de direção usam esta para, de fato, criar o caminho.
criarCaminho:: Int -> Int -> [[Int]] -> [[Int]]
criarCaminho linha coluna matriz = [matriz!!x | x<-[0..(linha-1)]] ++ [[matriz!!linha!!x|x<-[0..(coluna-1)]] ++ [0] ++ [matriz!!linha!!x|x<-[(coluna+1)..(length(matriz!!linha)-1)]]] ++ [matriz!!x | x<-[(linha+1)..(length(matriz)-1)]]


--Descorbir se o novo caminho criado gera um quadrado. Se não gerar quadrado (semQuadrado), retorna True.
semQuadrado:: Int -> Int -> [[Int]] -> Bool
semQuadrado 0 0 matriz = not(((matriz!!0!!1)==0) && ((matriz!!1!!0)==0) && ((matriz!!1!!1)==0))--
semQuadrado 0 coluna matriz = if not(coluna==length(matriz)-1) then not((((matriz!!0!!(coluna-1))==0) && ((matriz!!(1)!!coluna)==0) && ((matriz!!(1)!!(coluna-1))==0)) || (((matriz!!0!!(coluna+1))==0) && ((matriz!!(1)!!coluna)==0) && ((matriz!!(1)!!(coluna+1))==0)) ) else not(( ((matriz!!0!!(coluna-1))==0) && ((matriz!!(1)!!coluna)==0) && ((matriz!!(1)!!(coluna-1))==0)))
semQuadrado linha 0 matriz = if not(linha==length(matriz)-1) then not((((matriz!!linha!!(1))==0) && ((matriz!!(linha+1)!!0)==0) && ((matriz!!(linha+1)!!(1))==0)) || (((matriz!!linha!!(1))==0) && ((matriz!!(linha-1)!!0)==0) && ((matriz!!(linha-1)!!(1))==0)) ) else not(( ((matriz!!linha!!(1))==0) && ((matriz!!(linha-1)!!0)==0) && ((matriz!!(linha-1)!!(1))==0)))
semQuadrado linha coluna matriz =  if((coluna==length(matriz)-1) && (linha==length(matriz)-1)) then not((matriz!!(linha-1)!!coluna==0) && (matriz!!(linha-1)!!(coluna-1)==0) && (matriz!!linha!!(coluna-1)==0)) else if(coluna==length(matriz)-1) then not(((matriz!!(linha-1)!!coluna==0) && (matriz!!(linha-1)!!(coluna-1)==0) && (matriz!!linha!!(coluna-1)==0)) || ((matriz!!(linha+1)!!coluna==0) && (matriz!!(linha+1)!!(coluna-1)==0) && (matriz!!linha!!(coluna-1)==0))) else if(linha==length(matriz)-1) then not((((matriz!!(linha-1)!!(coluna)==0)) && (matriz!!linha!!(coluna-1)==0) && (matriz!!(linha-1)!!(coluna-1)==0)) || ((matriz!!(linha-1)!!(coluna)==0) && (matriz!!(linha-1)!!(coluna+1)==0) && (matriz!!linha!!(coluna+1)==0))) else not((((matriz!!(linha-1)!!(coluna)==0)) && (matriz!!linha!!(coluna-1)==0) && (matriz!!(linha-1)!!(coluna-1)==0)) || ((matriz!!(linha-1)!!(coluna)==0) && (matriz!!(linha-1)!!(coluna+1)==0) && (matriz!!linha!!(coluna+1)==0)) || ((matriz!!(linha+1)!!coluna==0) && (matriz!!linha!!(coluna-1)==0) && (matriz!!(linha+1)!!(coluna-1)==0)) || ((matriz!!(linha+1)!!coluna==0) && (matriz!!linha!!(coluna+1)==0) && (matriz!!(linha+1)!!(coluna+1)==0)) )


--  >>>>  "Interface" para criar caminhos em direções específicas.  <<<<

--Criar caminho para a esquerda
criarCaminhoEsquerda:: Int -> Int -> [[Int]] -> [[Int]]
criarCaminhoEsquerda linha coluna matriz
	|coluna==0 = matriz
	|otherwise = criarCaminho linha (coluna-1) matriz

--Criar caminho para a direita
criarCaminhoDireita:: Int -> Int -> [[Int]] -> [[Int]]
criarCaminhoDireita linha coluna matriz
	|coluna==(length(matriz)-1) = matriz
	|otherwise = criarCaminho linha (coluna+1) matriz


--Criar caminho para cima
criarCaminhoCima:: Int -> Int -> [[Int]] -> [[Int]]
criarCaminhoCima linha coluna matriz
	|linha==0 = matriz
	|otherwise = criarCaminho (linha-1) coluna matriz


--Criar caminho baixo
criarCaminhoBaixo:: Int -> Int -> [[Int]] -> [[Int]]
criarCaminhoBaixo linha coluna matriz
	|linha==(length(matriz)-1) = matriz
	|otherwise = criarCaminho (linha+1) coluna matriz

-- criarLabirinto linha coluna matriz randoms


--Estas funções criam os caminhos em uma matriz.
criarCaminhoPrincipal:: Int -> Int -> [[Int]] -> [Int] -> [[Int]]
criarCaminhoPrincipal linha coluna matriz random 
	|(coluna==(length(matriz)-1)) && (linha==(length(matriz)-1)) = criarCaminho linha coluna (criarCaminho 0 0 matriz)
	|((mod (head(random)) 4) == 0) && not(linha==length(matriz)-1) && not(linha==0) && not(coluna==0) && (semQuadrado linha (coluna-1) matriz) && not(matriz!!(linha)!!(coluna-1)==0)= criarCaminhoPrincipal linha (coluna-1) (criarCaminhoEsquerda linha (coluna) matriz) (tail(random)) --Esquerda
	|((mod (head(random)) 4) == 1) && not(coluna==length(matriz)-1) && (semQuadrado linha (coluna+1) matriz) && not(matriz!!(linha)!!(coluna+1)==0)= criarCaminhoPrincipal linha (coluna+1) (criarCaminhoDireita linha (coluna) matriz) (tail(random)) --Direita
	|((mod (head(random)) 4) == 2) && not(coluna==0) && not(coluna==length(matriz)-1) && not(linha==0) && (semQuadrado (linha-1) (coluna) matriz) && not(matriz!!(linha-1)!!(coluna)==0) = criarCaminhoPrincipal (linha-1) (coluna) (criarCaminhoCima (linha) (coluna) matriz) (tail(random)) --Cima
	|((mod (head(random)) 4) == 3) && not(linha==length(matriz)-1) && (semQuadrado (linha+1) (coluna) matriz) && not(matriz!!(linha+1)!!(coluna)==0) = criarCaminhoPrincipal (linha+1) (coluna) (criarCaminhoBaixo (linha) (coluna) matriz) (tail(random)) --Baixo
	|otherwise = criarCaminhoPrincipal linha coluna matriz (tail(random))


criarCaminhoPrincipalSimples linha coluna matriz (r:random)
	|(coluna==(length(matriz)-1)) && (linha==(length(matriz)-1)) = criarCaminho linha coluna (criarCaminho 0 0 matriz)
	|((mod r 2) == 0) && (coluna==length(matriz)-1) = criarCaminhoPrincipalSimples linha coluna (criarCaminho linha coluna matriz) random
	|((mod r 2) == 0) = criarCaminhoPrincipalSimples linha (coluna+1) (criarCaminho linha coluna matriz) random
	|((mod r 2) == 1) && (linha==length(matriz)-1) = criarCaminhoPrincipalSimples linha coluna (criarCaminho linha coluna matriz) random
	|((mod r 2) == 1) = criarCaminhoPrincipalSimples (linha+1) coluna (criarCaminho linha coluna matriz) random
	|otherwise = criarCaminhoPrincipalSimples linha coluna matriz random


existeCaminhoSecundario linha coluna matriz
	|(linha==0) && (coluna==0) = not(matriz!!0!!1==0) || not(matriz!!1!!0==0)
	|(linha==0) && (coluna==(length(matriz)-1)) = not(matriz!!linha!!(coluna-1)==0) || not(matriz!!(linha+1)!!(coluna)==0)
	|(coluna==0) && (linha==(length(matriz)-1)) = not(matriz!!linha!!(coluna+1)==0) || not(matriz!!(linha-1)!!(coluna)==0)
	|(linha==0) = not(matriz!!linha!!(coluna+1)==0) || not(matriz!!linha!!(coluna-1)==0) || not(matriz!!(linha+1)!!(coluna)==0) 
	|(linha==length(matriz)-1) = not(matriz!!linha!!(coluna+1)==0) || not(matriz!!linha!!(coluna-1)==0) || not(matriz!!(linha-1)!!(coluna)==0)
	|(coluna==0) = not(matriz!!linha!!(coluna+1)==0) || not(matriz!!(linha+1)!!(coluna)==0) || not(matriz!!(linha-1)!!(coluna)==0)
	|(coluna==length(matriz)-1) = not(matriz!!linha!!(coluna-1)==0) || not(matriz!!(linha+1)!!(coluna)==0) || not(matriz!!(linha-1)!!(coluna)==0)
	|otherwise = not(matriz!!linha!!(coluna+1)==0) || not(matriz!!linha!!(coluna-1)==0) || not(matriz!!(linha+1)!!(coluna)==0) || not(matriz!!(linha-1)!!(coluna)==0)
	
criarCaminhoSecundario:: Int -> Int -> [[Int]] -> [Int] -> [[Int]]
criarCaminhoSecundario linha coluna matriz (r:random)
	|(linha==(length(matriz))-1) && (coluna==(length(matriz))-1) = matriz
	|not(existeCaminhoSecundario linha coluna matriz) = matriz
	|(coluna==(length(matriz)-1)) && (linha==(length(matriz)-1)) = criarCaminho linha coluna (criarCaminho 0 0 matriz)
	|not(coluna==length(matriz)-1) && not(semQuadrado (linha  ) (coluna+1) matriz) && ((mod r 4)==1) = matriz
	|(linha>=0) && (coluna>=0) && not(coluna==length(matriz)-1) && (semQuadrado (linha  ) (coluna+1) matriz) && ((mod r 4)==1) && not(matriz!!(linha)!!(coluna+1)==0) = criarCaminhoSecundario linha (coluna+1) (criarCaminhoDireita linha (coluna) matriz) random --Direita
	|not(coluna==0) && not(semQuadrado (linha  ) (coluna-1) matriz) && ((mod r 4)==0) = matriz
	|(linha>=0) && (coluna>=0) && not(coluna==0) && (semQuadrado (linha  ) (coluna-1) matriz) && not(matriz!!linha!!(coluna-1)==0) && ((mod r 4)==0) = criarCaminhoSecundario linha (coluna-1) (criarCaminhoEsquerda linha (coluna) matriz) random --Esquerda
	|not(linha==length(matriz)-1) && not(semQuadrado (linha+1) (coluna  ) matriz) && ((mod r 4)==2) = matriz
	|(linha>=0) && (coluna>=0) && not(linha==length(matriz)-1) && (semQuadrado (linha+1) (coluna  ) matriz) && ((mod r 4)==2) && not(matriz!!(linha+1)!!coluna==0) = criarCaminhoSecundario (linha+1) (coluna) (criarCaminhoBaixo (linha) (coluna) matriz) random --Baixo
	|not(linha==0) && not(semQuadrado (linha-1) (coluna  ) matriz) && ((mod r 4)==3) = matriz
	|(linha>=0) && (coluna>=0) && not(linha==0) && (semQuadrado (linha-1) (coluna  ) matriz) && ((mod r 4)==3) && not(matriz!!(linha-1)!!coluna==0) = criarCaminhoSecundario (linha-1) (coluna) (criarCaminhoCima (linha) (coluna) matriz) random --Cima
	|otherwise = criarCaminhoSecundario linha coluna matriz random



criarTodosCaminhosSecundarios::[[Int]] -> [Int] -> [[Int]]
criarTodosCaminhosSecundarios matriz (r:random)
	|porcentagemBranco matriz<60 = criarTodosCaminhosSecundarios (criarCaminhoSecundario ((getBrancoIndice ((mod r ((qBranco 0 0 matriz)-1))+1) 0 0 matriz)!!0) ((getBrancoIndice ((mod r ((qBranco 0 0 matriz)-1) )+1) 0 0 matriz)!!1) matriz random) random
	|otherwise = matriz


-- Parametro "random" é gerado a partir do "let random = randoms g::[Int]" e é o que fornce os valores randômicos ao labirinto.
criarLabirinto:: Int -> [Int] -> [[Int]]
criarLabirinto n random = criarTodosCaminhosSecundarios (criarCaminhoPrincipalSimples 0 0 (gerarMatriz n) random) random


--             >>>>   Funções relacionadas ao herói   <<<<          

-- Cria um herói no labirinto na linha e coluna especificadas. Valor default é 0 0.
criarHeroi:: Int -> Int -> [[Int]] -> [[Int]]
criarHeroi linha coluna matriz = [matriz!!x | x<-[0..(linha-1)]] ++ [[matriz!!linha!!x|x<-[0..(coluna-1)]] ++ [2] ++ [matriz!!linha!!x|x<-[(coluna+1)..(length(matriz!!linha)-1)]]] ++ [matriz!!x | x<-[(linha+1)..(length(matriz)-1)]]


removerHeroi::[[Int]] -> [[Int]]
removerHeroi matriz = [matriz!!x | x<-[0..((getLinhaAtual matriz)-1)]] ++ [[matriz!!(getLinhaAtual matriz)!!x|x<-[0..((getColunaAtual matriz)-1)]] ++ [0] ++ [matriz!!(getLinhaAtual matriz)!!x|x<-[((getColunaAtual matriz)+1)..(length(matriz!!(getLinhaAtual matriz))-1)]]] ++ [matriz!!x | x<-[((getLinhaAtual matriz)+1)..(length(matriz)-1)]]


removerHeroi2::[[Int]] -> [[Int]]
removerHeroi2 matriz = [matriz!!x | x<-[0..((getLinhaAtual matriz)-1)]] ++ [[matriz!!(getLinhaAtual matriz)!!x|x<-[0..((getColunaAtual matriz)-1)]] ++ [4] ++ [matriz!!(getLinhaAtual matriz)!!x|x<-[((getColunaAtual matriz)+1)..(length(matriz!!(getLinhaAtual matriz))-1)]]] ++ [matriz!!x | x<-[((getLinhaAtual matriz)+1)..(length(matriz)-1)]]



removerHeroi3::[[Int]] -> [[Int]]
removerHeroi3 matriz = [matriz!!x | x<-[0..((getLinhaAtual matriz)-1)]] ++ [[matriz!!(getLinhaAtual matriz)!!x|x<-[0..((getColunaAtual matriz)-1)]] ++ [5] ++ [matriz!!(getLinhaAtual matriz)!!x|x<-[((getColunaAtual matriz)+1)..(length(matriz!!(getLinhaAtual matriz))-1)]]] ++ [matriz!!x | x<-[((getLinhaAtual matriz)+1)..(length(matriz)-1)]]


-- Converter tipo MaybeInt do haskell para um Int (detalhes de haskell). Converter retorno da função que encontra o indíce de um determinado elemento em uma lista.
maybeIntToInt (Just a) = a


-- Valor atual da linha em que o herói se encontra
getLinhaAtual::[[Int]] -> Int
getLinhaAtual matriz = maybeIntToInt (elemIndex (head([x|x<-matriz, elem 2 x])) matriz)


-- Valor atual da coluna em que o herói se encontra
getColunaAtual::[[Int]] -> Int
getColunaAtual matriz = maybeIntToInt (elemIndex 2 (head([x|x<-matriz, elem 2 x])) )




-- Esta função muda a posição do herói no labirinto. 
caminharPara:: Int -> Int -> [[Int]] -> [[Int]]
caminharPara linha coluna matriz = [matriz!!x | x<-[0..(linha-1)]] ++ [[matriz!!linha!!x|x<-[0..(coluna-1)]] ++ [2] ++ [matriz!!linha!!x|x<-[(coluna+1)..(length(matriz!!linha)-1)]]] ++ [matriz!!x | x<-[(linha+1)..(length(matriz)-1)]]


caminharPara2:: [[Int]] -> [Int] -> [[Int]]
caminharPara2 matriz [linha,coluna]  = [(removerHeroi2 matriz)!!x | x<-[0..(linha-1)]] ++ [[(removerHeroi2 matriz)!!linha!!x|x<-[0..(coluna-1)]] ++ [2] ++ [(removerHeroi2 matriz)!!linha!!x|x<-[(coluna+1)..(length((removerHeroi2 matriz)!!linha)-1)]]] ++ [(removerHeroi2 matriz)!!x | x<-[(linha+1)..(length((removerHeroi2 matriz))-1)]]



caminharPara5:: [[Int]] -> [Int] -> [[Int]]
caminharPara5 matriz [linha,coluna]  = [(removerHeroi3 matriz)!!x | x<-[0..(linha-1)]] ++ [[(removerHeroi3 matriz)!!linha!!x|x<-[0..(coluna-1)]] ++ [2] ++ [(removerHeroi3 matriz)!!linha!!x|x<-[(coluna+1)..(length((removerHeroi3 matriz)!!linha)-1)]]] ++ [(removerHeroi3 matriz)!!x | x<-[(linha+1)..(length((removerHeroi3 matriz))-1)]]





--     >>>>  Função Heurística  <<<<

heuristica:: [[Int]] -> Int
heuristica matriz = ((length(matriz)-1)-(getLinhaAtual matriz)) + ((length(matriz)-1)-(getColunaAtual matriz))


getMenorHeuristica::[[Int]] -> [[Int]] -> Int
--getMenorHeuristica [] lista = []
getMenorHeuristica matriz lista = maybeIntToInt (elemIndex (minimum(map (heuristica) (map (caminharPara2 matriz) lista))) (map (heuristica) (map (caminharPara2 matriz) lista)))

-- Listar caminhos possíveis
listaCaminhosPossiveis matriz
	|(getLinhaAtual matriz==0)&&(getColunaAtual matriz==0) = [x|x<-[[0,1],[1,0]], (matriz!!(x!!0)!!(x!!1)==0)]
	|(getLinhaAtual matriz==(length(matriz)-1))&&(getColunaAtual matriz==(length(matriz)-1)) = [x|x<-[ [(length(matriz)-1),(length(matriz)-1-1)],[(length(matriz)-1-1),(length(matriz)-1)]], (matriz!!(x!!0)!!(x!!1)==0)]
	|(getLinhaAtual matriz==0)&&(getColunaAtual matriz==(length(matriz)-1)) = [x|x<-[[1,length(matriz)-1],[0,length(matriz)-1-1]], (matriz!!(x!!0)!!(x!!1)==0)]
	|(getLinhaAtual matriz==(length(matriz)-1))&&(getColunaAtual matriz==0) = [x|x<-[[length(matriz)-1,1],[length(matriz)-1-1,0]], (matriz!!(x!!0)!!(x!!1)==0)]
	|(getLinhaAtual matriz==0) = [x|x<-[[0,(getColunaAtual matriz)-1],[0,(getColunaAtual matriz)+1],[1,getColunaAtual matriz]], (matriz!!(x!!0)!!(x!!1)==0)]
	|(getLinhaAtual matriz==length(matriz)-1) = [x|x<-[[length(matriz)-1,(getColunaAtual matriz)-1],[length(matriz)-1,(getColunaAtual matriz)+1],[length(matriz)-1-1,getColunaAtual matriz]], (matriz!!(x!!0)!!(x!!1)==0)]--length y
	|(getColunaAtual matriz==0) = [x|x<-[[(getLinhaAtual matriz)+1,0],[(getLinhaAtual matriz)-1,0],[getLinhaAtual matriz,1]], (matriz!!(x!!0)!!(x!!1)==0)]--y 0
	|(getColunaAtual matriz==length(matriz)-1) = [x|x<-[[(getLinhaAtual matriz)+1,length(matriz)-1],[(getLinhaAtual matriz)-1,length(matriz)-1],[(getLinhaAtual matriz), length(matriz)-1-1]],(matriz!!(x!!0)!!(x!!1)==0)]--y length
	|otherwise = [x|x<-[[(getLinhaAtual matriz)-1,(getColunaAtual matriz)],[(getLinhaAtual matriz)+1,(getColunaAtual matriz)],[(getLinhaAtual matriz),(getColunaAtual matriz)-1],[(getLinhaAtual matriz),(getColunaAtual matriz)+1]],(matriz!!(x!!0)!!(x!!1)==0)]--otherwise


listaCaminhosPossiveis2 matriz verticesVisitados
	|(getLinhaAtual matriz==0)&&(getColunaAtual matriz==0) = [x|x<-[[0,1],[1,0]], (matriz!!(x!!0)!!(x!!1)==0), not(elem x verticesVisitados)]
	|(getLinhaAtual matriz==(length(matriz)-1))&&(getColunaAtual matriz==(length(matriz)-1)) = [x|x<-[ [(length(matriz)-1),(length(matriz)-1-1)],[(length(matriz)-1-1),(length(matriz)-1)]], (matriz!!(x!!0)!!(x!!1)==0), not(elem x verticesVisitados)]
	|(getLinhaAtual matriz==0)&&(getColunaAtual matriz==(length(matriz)-1)) = [x|x<-[[1,length(matriz)-1],[0,length(matriz)-1-1]], (matriz!!(x!!0)!!(x!!1)==0), not(elem x verticesVisitados)]
	|(getLinhaAtual matriz==(length(matriz)-1))&&(getColunaAtual matriz==0) = [x|x<-[[length(matriz)-1,1],[length(matriz)-1-1,0]], (matriz!!(x!!0)!!(x!!1)==0), not(elem x verticesVisitados)]
	|(getLinhaAtual matriz==0) = [x|x<-[[0,(getColunaAtual matriz)-1],[0,(getColunaAtual matriz)+1],[1,getColunaAtual matriz]], (matriz!!(x!!0)!!(x!!1)==0), not(elem x verticesVisitados)]
	|(getLinhaAtual matriz==length(matriz)-1) = [x|x<-[[length(matriz)-1,(getColunaAtual matriz)-1],[length(matriz)-1,(getColunaAtual matriz)+1],[length(matriz)-1-1,getColunaAtual matriz]], (matriz!!(x!!0)!!(x!!1)==0), not(elem x verticesVisitados)]--length y
	|(getColunaAtual matriz==0) = [x|x<-[[(getLinhaAtual matriz)+1,0],[(getLinhaAtual matriz)-1,0],[getLinhaAtual matriz,1]], (matriz!!(x!!0)!!(x!!1)==0), not(elem x verticesVisitados)]--y 0
	|(getColunaAtual matriz==length(matriz)-1) = [x|x<-[[(getLinhaAtual matriz)+1,length(matriz)-1],[(getLinhaAtual matriz)-1,length(matriz)-1],[(getLinhaAtual matriz), length(matriz)-1-1]],(matriz!!(x!!0)!!(x!!1)==0), not(elem x verticesVisitados)]--y length
	|otherwise = [x|x<-[[(getLinhaAtual matriz)-1,(getColunaAtual matriz)],[(getLinhaAtual matriz)+1,(getColunaAtual matriz)],[(getLinhaAtual matriz),(getColunaAtual matriz)-1],[(getLinhaAtual matriz),(getColunaAtual matriz)+1]],(matriz!!(x!!0)!!(x!!1)==0), not(elem x verticesVisitados)]--otherwise

	

--ordenar
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
	where
		lesser = [y|y<-xs, y<p]
		greater = [y|y<-xs, y>=p]

-- Ordenar pela Heurística
qsortHeuristica:: [[Int]] -> [[Int]] -> [[Int]]
qsortHeuristica matriz [] = []
qsortHeuristica matriz (p:xs) = (qsortHeuristica matriz lesser) ++ [p] ++ (qsortHeuristica matriz greater)
	where
		lesser = [y|y<-xs, heuristica(caminharPara2 matriz y)<heuristica(caminharPara2 matriz p)]
		greater = [y|y<-xs, heuristica(caminharPara2 matriz y)>=heuristica(caminharPara2 matriz p)]

-- Ordenar pela heurística e função G.
qsortHeuristicaFuncaoG:: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
qsortHeuristicaFuncaoG matriz verticesVisitados [] = []
qsortHeuristicaFuncaoG matriz verticesVisitados (p:xs) = (qsortHeuristicaFuncaoG matriz verticesVisitados lesser) ++ [p] ++ (qsortHeuristicaFuncaoG matriz verticesVisitados greater)
	where
		lesser = [y|y<-xs, ((heuristica(caminharPara2 matriz y))+(funcaoG (caminharPara2 matriz y) (verticesVisitados)))< ((heuristica(caminharPara2 matriz p))+(funcaoG (caminharPara2 matriz p) (verticesVisitados)))]
		greater =[y|y<-xs, ((heuristica(caminharPara2 matriz y))+(funcaoG (caminharPara2 matriz y) (verticesVisitados)))>=((heuristica(caminharPara2 matriz p))+(funcaoG (caminharPara2 matriz p) (verticesVisitados)))]


--FuncaoG
funcaoG matriz verticesVisitados = length(solucao matriz verticesVisitados)


-- Descobre se dois lugares são adjacentes
eAdjacente:: [Int]-> [Int] -> Bool
eAdjacente [linha1,coluna1] [linha2,coluna2]
	|(((linha1-linha2)==1) || ((linha2-linha1)==1)) && (coluna1==coluna2) = True
	|(((coluna1-coluna2)==1) || ((coluna2-coluna1)==1)) && (linha1==linha2) = True 
	|otherwise = False


seqAdj matriz verticesVisitados lista
	|verticesVisitados==[] = lista
	|lista==[] = seqAdj matriz (reverse(verticesVisitados)) [head(reverse(verticesVisitados))]
	|eAdjacente (head(verticesVisitados)) (last(lista)) = seqAdj matriz (tail(verticesVisitados)) (lista++[head(verticesVisitados)])
	|otherwise = seqAdj matriz (tail(verticesVisitados)) (lista)

solucao matriz verticesVisitados = removerHeroi3(foldl (caminharPara5) (matriz) (seqAdj matriz verticesVisitados []))



-- >>>>>> Buscas <<<<<<<<<

-- Busca em Largura
buscaEmLargura::[[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
buscaEmLargura matriz verticesVisitados fila
	|matriz!!(length(matriz)-1)!!(length(matriz)-1)==2 = (solucao matriz verticesVisitados) -- ++ nub(verticesVisitados) -- ++ [[length(matriz)-1,length(matriz)-1]]
	|fila==[] = buscaEmLargura matriz [[0,0]] (listaCaminhosPossiveis2 matriz verticesVisitados)
	|otherwise = buscaEmLargura (caminharPara2 matriz ((fila)!!0)) (verticesVisitados++([(fila)!!0])) (tail(fila)++(listaCaminhosPossiveis2 matriz verticesVisitados))


-- Busca Gulosa
buscaGulosa::[[Int]]->[[Int]]->[[Int]]->[[Int]]
buscaGulosa matriz verticesVisitados fila
	|heuristica matriz==0 = (solucao matriz verticesVisitados) -- ++ nub(verticesVisitados) -- ++ [[length(matriz)-1,length(matriz)-1]]
	|fila==[] = buscaGulosa matriz [[0,0]] (qsortHeuristica matriz (listaCaminhosPossiveis2 matriz verticesVisitados))
	|otherwise = buscaGulosa (caminharPara2 matriz ((fila)!!0)) (verticesVisitados++([(fila)!!0])) ((qsortHeuristica matriz (listaCaminhosPossiveis2 matriz verticesVisitados))++tail(fila))

-- Busca A*
buscaAEstrela::[[Int]]->[[Int]]->[[Int]]->[[Int]]
buscaAEstrela matriz verticesVisitados fila
	|heuristica matriz==0 = (solucao matriz verticesVisitados) -- ++ nub(verticesVisitados) -- ++ [[length(matriz)-1,length(matriz)-1]]
	|fila==[] = buscaAEstrela matriz [[0,0]] (qsortHeuristicaFuncaoG matriz verticesVisitados (listaCaminhosPossiveis2 matriz verticesVisitados))
	|otherwise = buscaAEstrela (caminharPara2 matriz ((fila)!!0)) (verticesVisitados++([(fila)!!0])) ((qsortHeuristicaFuncaoG matriz verticesVisitados ((listaCaminhosPossiveis2 matriz verticesVisitados)++tail(fila))))

-- Numero de estados visitados
estadosVisitados [] = 0
estadosVisitados (m:matriz) = length(filter (==4) m) + length(filter (==5) m) + estadosVisitados matriz

-- Funcão que cria o labirinto
labirinto tamanho random = criarHeroi 0 0 (criarLabirinto tamanho random) 

-- Função que aplica uma busca f (que pode ser buscaEmLargura, buscaGulosa, buscaAEstela) a um labirinto
busca f labirinto = imprimir ((f labirinto [] []) ++ [[estadosVisitados (f labirinto [] [])]])

-- função que imprime um labirinto
imprimir x = putStr (foldl1 (++) (map (++"\n") (map (show) x)))


