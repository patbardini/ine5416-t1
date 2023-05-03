module Backtracking where
import Matriz
import Resolvedor
import Data.Tuple
import Debug.Trace

backTracking :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Intervalo -> (Bool, MatrizValores)
backTracking matrizOperadores matrizValores (linha, coluna) valorSelecionado (menor, maior) = do
    if linha == length matrizValores then
        (True, matrizValores)
    else
        if coluna < length matrizValores then
            let (menor1, maior1) = encontraIntervalo matrizOperadores matrizValores (linha, coluna) (1, length matrizValores)
            in solucionar matrizOperadores matrizValores (linha, coluna) menor1 (menor1, maior1)
            -- in if ehMaiorQueTodosVizinhos matrizOperadores (getPosicaoEquivalenteMatrizOperadores matrizOperadores (linha, coluna)) then
            --     solucionarMaior matrizOperadores matrizValores (linha, coluna) maior1 (menor1, maior1)
            -- else
            --     solucionarMenor matrizOperadores matrizValores (linha, coluna) (menor1 - 1) (menor1, maior1)
        else
            backTracking matrizOperadores matrizValores ((linha+1), 0) valorSelecionado (menor, maior)

-- solucionarMenor :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Intervalo -> (Bool, MatrizValores)
-- solucionarMenor matrizOperadores matrizValores (linha, coluna) valorSelecionado (menor, maior) = do
--     if valorSelecionado > maior then
--         (False, matrizValores)
--     else            
--         if getPosicaoNumeroValido matrizOperadores  matrizValores (linha, coluna) valorSelecionado then
--             let (valido, matriz) = backTracking matrizOperadores (trace (gridToString matrizValores) (inserirValorMatriz matrizValores (linha, coluna) valorSelecionado)) (linha, (coluna+1)) 1 (menor, maior)
--             in if valido then
--                 (True, matriz)
--             else solucionarMenor matrizOperadores matrizValores (linha, coluna) (valorSelecionado + 1) (menor, maior)
--         else
--             solucionarMenor matrizOperadores matrizValores (linha, coluna) (valorSelecionado + 1) (menor, maior)

-- solucionarMaior :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Intervalo -> (Bool, MatrizValores)
-- solucionarMaior matrizOperadores matrizValores (linha, coluna) valorSelecionado (menor, maior) = do
--     if valorSelecionado < menor then
--         (False, matrizValores)
--     else            
--         if getPosicaoNumeroValido matrizOperadores  matrizValores (linha, coluna) valorSelecionado then
--             let (valido, matriz) = backTracking matrizOperadores (trace (gridToString matrizValores) (inserirValorMatriz matrizValores (linha, coluna) valorSelecionado)) (linha, (coluna+1)) 1 (menor, maior)
--             in if valido then
--                 (True, matriz)
--             else solucionarMaior matrizOperadores matrizValores (linha, coluna) (valorSelecionado - 1) (menor, maior)
--         else
--             solucionarMaior matrizOperadores matrizValores (linha, coluna) (valorSelecionado - 1) (menor, maior)

solucionar :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Intervalo -> (Bool, MatrizValores)
solucionar matrizOperadores matrizValores (linha, coluna) valorSelecionado (menor, maior) = do
    if valorSelecionado > maior then
        (False, matrizValores)
    else            
        if getPosicaoNumeroValido matrizOperadores  matrizValores (linha, coluna) valorSelecionado then
            let (valido, matriz) = backTracking matrizOperadores (trace (gridToString matrizValores) (inserirValorMatriz matrizValores (linha, coluna) valorSelecionado)) (linha, (coluna+1)) 1 (menor, maior)
            in if valido then
                (True, matriz)
            else solucionar matrizOperadores matrizValores (linha, coluna) (valorSelecionado + 1) (menor, maior)
        else
            solucionar matrizOperadores matrizValores (linha, coluna) (valorSelecionado + 1) (menor, maior)
