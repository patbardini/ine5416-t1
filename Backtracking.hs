module Backtracking where
import Matriz
import Resolvedor
import Data.Tuple
import Debug.Trace


-- backTrackingMenor :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> (Bool, MatrizValores)
-- backTrackingMenor matrizOperadores matrizValores (linha, coluna) valorSelecionado = do
--     if linha == length matrizValores then
--         (True, matrizValores)
--         -- let (valido, matriz) = backtrancking matrizOperadores (inserirValorMatriz matrizValores (linha, coluna) valorSelecionado) (0, 0) 1
--         -- in if valido then
--         --         (True, matriz)
--         -- else backTrackingMenor matrizOperadores matrizValores (linha, coluna) (valorSelecionado + 1)
--     else
--         if coluna < length matrizValores then
--             putMenorValor matrizOperadores matrizValores (linha, coluna) valorSelecionado
--         else
--             backTrackingMenor matrizOperadores (matrizValores) (linha + 1, 0) 1

-- putMenorValor :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> (Bool, MatrizValores)
-- putMenorValor matrizOperadores matrizValores (linha, coluna) valorSelecionado = do
--     if valorSelecionado > (length matrizValores) then
--         (False, matrizValores)
--     else
--         if ehMenorQueTodosVizinhos matrizOperadores ( getPosicaoEquivalenteMatrizOperadores matrizOperadores (linha, coluna) ) then
--             if valorSelecionado > (length matrizValores) then
--                 (False, matrizValores)
--             else
--                 if getPosicaoNumeroValido matrizOperadores  matrizValores (linha, coluna) valorSelecionado then
--                     let (valido, matriz) = backTrackingMenor matrizOperadores (trace (gridToString matrizValores) (inserirValorMatriz matrizValores (linha, coluna) valorSelecionado)) (linha, coluna+1) 1
--                     in if valido then
--                         (True, matriz)
--                     else
--                         putMenorValor matrizOperadores (matrizValores) (linha, coluna) (valorSelecionado + 1)
--                 else
--                     putMenorValor matrizOperadores (matrizValores) (linha, coluna) (valorSelecionado + 1)
--         else
--             putMenorValor matrizOperadores (matrizValores) (linha, coluna) (valorSelecionado + 1)

backtrancking :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> (Bool, MatrizValores)
backtrancking matrizOperadores matrizValores (linha, coluna) valorSelecionado = do
    if linha == length matrizValores then
        (True, matrizValores)
    else
        if coluna < length matrizValores then
            solucionar matrizOperadores matrizValores (linha, coluna) valorSelecionado
        else
            backtrancking matrizOperadores matrizValores ((linha+1), 0) valorSelecionado

solucionar :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> (Bool, MatrizValores)
solucionar matrizOperadores matrizValores (linha, coluna) valorSelecionado = do
    if valorSelecionado > (length matrizValores) then
        (False, matrizValores)
    else            
        if getPosicaoNumeroValido matrizOperadores  matrizValores (linha, coluna) valorSelecionado then
            let (valido, matriz) = backtrancking matrizOperadores (trace (gridToString matrizValores) (inserirValorMatriz matrizValores (linha, coluna) valorSelecionado)) (linha, (coluna+1)) 1
            in if valido then
                (True, matriz)
            else solucionar matrizOperadores matrizValores (linha, coluna) (valorSelecionado + 1)
        else
            solucionar matrizOperadores matrizValores (linha, coluna) (valorSelecionado + 1)
