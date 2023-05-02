module Backtracking where
import Matriz
import Resolvedor
import Data.Tuple
import Debug.Trace

-- Realiza a chamada recursiva do solucionador do problema, avançando as linhas conforme chega ao final da coluna
backTracking :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Intervalo -> (Bool, MatrizValores)
backTracking matrizOperadores matrizValores (linha, coluna) valorSelecionado (menor, maior) = do
    if linha == length matrizValores then
        (True, matrizValores)
    else
        if coluna < length matrizValores then
            -- O intervalo de chute da posição atual da matriz é difinido
            let (menor1, maior1) = encontraIntervalo matrizOperadores matrizValores (linha, coluna) (1, length matrizValores)
            in solucionar matrizOperadores matrizValores (linha, coluna) menor1 (menor1, maior1)
        else
            -- Chama o backtracking para a próxima linha da matriz
            backTracking matrizOperadores matrizValores ((linha+1), 0) valorSelecionado (menor, maior)

-- Teste todos os valores possíveis para uma posição da matriz
solucionar :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Intervalo -> (Bool, MatrizValores)
solucionar matrizOperadores matrizValores (linha, coluna) valorSelecionado (menor, maior) = do
    if valorSelecionado > maior then
        (False, matrizValores)
    else
        -- Verifica se é possível colocar o número Selecionado na posição atual da matriz            
        if getPosicaoNumeroValido matrizOperadores  matrizValores (linha, coluna) valorSelecionado then

            -- CASO QUEIRA VISUALIZAR AS MATRIZES INTERMEDIARIAS DA SOLUÇÃO, COMENTE A LINHA 30 E RETIRA O COMENTARIO DA 31
            let (valido, matriz) = backTracking matrizOperadores (inserirValorMatriz matrizValores (linha, coluna) valorSelecionado) (linha, (coluna+1)) 1 (menor, maior)            
            -- let (valido, matriz) = backTracking matrizOperadores (trace (gridToString matrizValores) (inserirValorMatriz matrizValores (linha, coluna) valorSelecionado)) (linha, (coluna+1)) 1 (menor, maior)
            
            -- Retorno quando a função bate no fundo
            in if valido then
                (True, matriz)
            -- Else vai chamar a função para o próximo valor possível
            else solucionar matrizOperadores matrizValores (linha, coluna) (valorSelecionado + 1) (menor, maior)
        else
            -- Avança o valor selecionado caso o atual não seja possível
            solucionar matrizOperadores matrizValores (linha, coluna) (valorSelecionado + 1) (menor, maior)
