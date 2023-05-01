module Backtracking where
import Matriz
import Resolvedor
import Data.Tuple


backtrancking :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> (Bool, MatrizValores)
backtrancking matrizOperadores matrizValores (linha, coluna) valorSelecionado = do
    if linha == length matrizValores then
        (True, matrizValores)
    else
        if coluna < length (matrizValores !! 0) then
            solucionar matrizOperadores matrizValores (linha, coluna) valorSelecionado
        else
            backtrancking matrizOperadores matrizValores ((linha+1), 0) valorSelecionado

solucionar :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> (Bool, MatrizValores)
solucionar matrizOperadores matrizValores (linha, coluna) valorInserido = do
    if valorInserido == 0 then
        (False, matrizValores)
    else
        if getPosicaoNumeroValido matrizOperadores  matrizValores (linha, coluna) valorInserido then
            let (valido, matriz) = backtrancking matrizOperadores (inserirValorMatriz matrizValores (linha, coluna) valorInserido) (linha, (coluna+1)) (length matrizValores)
            in if valido then
                (True, matriz)
            else solucionar matrizOperadores matrizValores (linha, coluna) (valorInserido - 1)
        else
            solucionar matrizOperadores matrizValores (linha, coluna) (valorInserido - 1)

