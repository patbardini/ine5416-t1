module Resolvedor where
import Matriz
import Data.Tuple

type Posicao = (Int, Int)
type Intervalo = (Int, Int)

getPosicoesDosX :: MatrizOperadores -> Posicao -> [Posicao] -> [Posicao]
getPosicoesDosX matriz (linha, coluna) listaPosicoes = do
    let dimensaoMatrizLinha = length (matriz)
    let dimensaoMatrizColuna = length (matriz!!linha)

    if linha > (dimensaoMatrizLinha-1) then
        listaPosicoes
    else if coluna <= (dimensaoMatrizColuna-1) then
        if (matriz!!linha!!coluna == 'x') then
            getPosicoesDosX matriz (linha, coluna+1) (listaPosicoes ++ [(linha, coluna)])
        else
            getPosicoesDosX matriz (linha, coluna+1) listaPosicoes
    else
        getPosicoesDosX matriz (linha+1, 0) listaPosicoes

getOperadorAcima :: MatrizOperadores -> Posicao -> Operador
getOperadorAcima matriz (linha, coluna) = 
    if (linha > 0) then
        if ((matriz!!(linha-1))!!coluna /= 'x') then
            (matriz!!(linha-1))!!coluna
        else
            '|'
    else
        '|'

getOperadorAbaixo :: MatrizOperadores -> Posicao -> Operador
getOperadorAbaixo matriz (linha, coluna) = 
    if (linha+1 < length matriz) then
        if ((matriz!!(linha+1))!!coluna /= 'x') then
            (matriz!!(linha+1))!!coluna
        else
            '|'
    else
        '|'

getOperadorAEsquerda :: MatrizOperadores -> Posicao -> Operador
getOperadorAEsquerda matriz (linha, coluna) =     
    if (coluna > 0) then
        if ((matriz!!linha)!!(coluna-1) /= 'x') then
            (matriz!!linha)!!(coluna-1)
        else
            '|'
    else
        '|'

getOperadorADireita :: MatrizOperadores -> Posicao -> Operador
getOperadorADireita matriz (linha, coluna) =
    let lengthMatriz = (length matriz)
        dimensaoMatriz = getDimensaoMatriz matriz
        colunaLength = if dimensaoMatriz == 6 
                          then (lengthMatriz - 1)
                          else lengthMatriz
    in
        if (coluna+1 < colunaLength) then
            if ((matriz!!linha)!!(coluna+1) /= 'x') then
                (matriz!!linha)!!(coluna+1)
            else
                '|'
        else
            '|'

ehMaiorQueTodosVizinhos :: MatrizOperadores -> Posicao -> Bool
ehMaiorQueTodosVizinhos matriz (linha, coluna) = do
    let operadorAcima = getOperadorAcima matriz (linha, coluna)
    let operadorAbaixo = getOperadorAbaixo matriz (linha, coluna)
    let operadorAEsquerda = getOperadorAEsquerda matriz (linha, coluna)
    let operadorADireita = getOperadorADireita matriz (linha, coluna)
    let elemento = matriz!!linha!!coluna

    if (elemento /= 'x') then
        False
    else if ((operadorAcima == '^' || operadorAcima == '|') 
          && (operadorAbaixo == 'v' || operadorAbaixo == '|')
          && (operadorAEsquerda == '<' || operadorAEsquerda == '|')
          && (operadorADireita == '>' || operadorADireita == '|')) then
            True
    else
        False

ehMenorQueTodosVizinhos :: MatrizOperadores -> Posicao -> Bool
ehMenorQueTodosVizinhos matriz (linha, coluna) = do
    let operadorAcima = getOperadorAcima matriz (linha, coluna)
    let operadorAbaixo = getOperadorAbaixo matriz (linha, coluna)
    let operadorAEsquerda = getOperadorAEsquerda matriz (linha, coluna)
    let operadorADireita = getOperadorADireita matriz (linha, coluna)
    let elemento = matriz!!linha!!coluna

    if (elemento /= 'x') then
        False
    else if ((operadorAcima == 'v' || operadorAcima == '|') 
          && (operadorAbaixo == '^' || operadorAbaixo == '|')
          && (operadorAEsquerda == '>' || operadorAEsquerda == '|')
          && (operadorADireita == '<' || operadorADireita == '|')) then
            True
    else
        False

getPosicaoEquivalenteMatrizOperadores :: MatrizOperadores -> Posicao -> Posicao
getPosicaoEquivalenteMatrizOperadores matrizOperadores (linha, coluna) = 
    (pedacosDe tamanhoMatriz posicoesDosX)!!linha!!coluna where
    tamanhoMatriz = getDimensaoMatriz matrizOperadores
    posicoesDosX = getPosicoesDosX matrizOperadores (0, 0) []

getRegiao :: MatrizValores -> Posicao -> [Int]
getRegiao matriz (linha, coluna) = 
    let (dimensaoLinha, dimensaoColuna) = getDimensaoRegiao (length matriz)
        (inicioLinha, inicioColuna) = ((linha `div` dimensaoLinha) * dimensaoLinha, (coluna `div` dimensaoColuna) * dimensaoColuna)
        (finalLinha, finalColuna) = (inicioLinha + dimensaoLinha - 1, inicioColuna + dimensaoColuna - 1)
        regiao = [matriz!!i !!j | i <- [inicioLinha .. finalLinha], 
                                  j <- [inicioColuna .. finalColuna]]
    in regiao

getPosicaoNumeroValido :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Bool
getPosicaoNumeroValido matrizOperadores matrizValores (linha, coluna) valorSelecionado = do
    let tamanhoMatriz = getDimensaoMatriz matrizOperadores
    if not (isRepetidoValorLinha matrizValores valorSelecionado (linha, 0) tamanhoMatriz)
        && not (isRepetidoValorColuna matrizValores valorSelecionado (0, coluna) tamanhoMatriz)
        && not (isRepetidoValorRegiao (getRegiao matrizValores (linha, coluna)) valorSelecionado) then
            True
    else
        False

isRepetidoValorLinha :: MatrizValores -> Int -> Posicao -> Int -> Bool
isRepetidoValorLinha matrizValores valorSelecionado (linha, coluna) qtdColunas =
    if coluna == qtdColunas then
        False
    else if (matrizValores !!linha !!coluna) == valorSelecionado then
        True
    else
        isRepetidoValorLinha matrizValores valorSelecionado (linha, (coluna + 1)) qtdColunas

isRepetidoValorColuna :: MatrizValores -> Int -> Posicao -> Int -> Bool
isRepetidoValorColuna matrizValores valorSelecionado (linha, coluna) qtdLinhas = 
    if linha == qtdLinhas then
        False
    else if (matrizValores !!linha !!coluna) == valorSelecionado then
        True
    else
        isRepetidoValorColuna matrizValores valorSelecionado ((linha + 1), coluna) qtdLinhas

isRepetidoValorRegiao :: [Int] -> Int -> Bool
isRepetidoValorRegiao [] _ = False
isRepetidoValorRegiao (x:xs) valorSelecionado = if x == valorSelecionado then True else isRepetidoValorRegiao xs valorSelecionado

-- Encontra o intervalo possível de valores naquela região
encontraIntervalo :: MatrizOperadores -> MatrizValores -> Posicao -> Intervalo -> Intervalo
encontraIntervalo matrizOperadores matrizValores (linha, coluna) (menor, maior) =
    let posicaoNaMatrizOperadores = getPosicaoEquivalenteMatrizOperadores matrizOperadores (linha, coluna)
        linhaMatrizOperadores = fst posicaoNaMatrizOperadores
        colunaMatrizOperadores = snd posicaoNaMatrizOperadores

        operadorAcima = getOperadorAcima matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)
        operadorAEsquerda = getOperadorAEsquerda matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)

        listaValidadeOperadores = [isOperadorValido operadorAcima,
                                   isOperadorValido operadorAEsquerda]

        -- Encontra a faixa de valores possíveis para posição (x, y)
        (menor1, maior1) = maiorMenor operadorAcima '^' (matrizValores !!(linha - 1) !!coluna) (menor, maior) (fst (listaValidadeOperadores!!0))
        (menor2, maior2) = maiorMenor operadorAEsquerda '<' (matrizValores !!linha !!(coluna - 1)) (menor1, maior1) (fst (listaValidadeOperadores!!1))
    
    in (menor2, maior2)

-- Devolve uma tupla com o valor (menor, maior) para o intervalo solicitado
maiorMenor :: Operador -> Char -> Int -> (Int, Int) -> Bool -> (Int, Int)
maiorMenor operador posicao valor (menor, maior) valido =      
    if valido then
        if valor == 0 then
            (menor, maior)
        else  
            if posicao == '^' then
                if operador == '>' || operador == '^' then
                    if menor <= valor then
                        (valor + 1, maior)
                    else
                        (menor, maior)
                else
                    if maior >= valor then
                        (menor, valor - 1)
                    else
                        (menor, maior)
            else
                if operador == '>' || operador == '^' then
                    if maior >= valor then
                        (menor, valor - 1)
                    else
                        (menor, maior)
                else
                    if menor <= valor then
                        (valor + 1, maior)
                    else
                        (menor, maior)
    else
        (menor, maior)


isOperadorValido :: Operador -> (Bool, Operador)
isOperadorValido operador = 
    case operador of
        'v' -> (True, operador)
        '^' -> (True, operador)
        '>' -> (True, operador)
        '<' -> (True, operador)
        _ -> (False, operador)
