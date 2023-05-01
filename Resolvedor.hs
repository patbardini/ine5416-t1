module Resolvedor where
import Matriz
import Data.Tuple

type Posicao = (Int, Int)

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
    if (coluna+1 < length matriz) then
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

    if (validaOperadores matrizOperadores matrizValores (linha, coluna) valorSelecionado)
        && not (isRepetidoValorLinha matrizValores valorSelecionado (linha, 0) tamanhoMatriz)
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

validaOperadores :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Bool
validaOperadores matrizOperadores matrizValores (linha, coluna) valorSelecionado =
    let posicaoNaMatrizOperadores = getPosicaoEquivalenteMatrizOperadores matrizOperadores (linha, coluna)
        linhaMatrizOperadores = fst posicaoNaMatrizOperadores
        colunaMatrizOperadores = snd posicaoNaMatrizOperadores

        operadorAcima = getOperadorAcima matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)
        operadorAbaixo = getOperadorAbaixo matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)
        operadorAEsquerda = getOperadorAEsquerda matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)
        operadorADireita = getOperadorADireita matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)

        listaValidadeOperadores = [isOperadorValido operadorAcima,
                                   isOperadorValido operadorAbaixo,
                                   isOperadorValido operadorAEsquerda,
                                   isOperadorValido operadorADireita]

        listaValidadeOperacao = concat [[validaOperacao operadorAcima valorSelecionado (matrizValores !!(linha - 1) !!coluna) | fst (listaValidadeOperadores!!0)],
                                        [validaOperacao operadorAbaixo (matrizValores !!(linha + 1) !!coluna) valorSelecionado | fst (listaValidadeOperadores!!1)],
                                        [validaOperacao operadorAEsquerda (matrizValores !!linha !!(coluna - 1)) valorSelecionado | fst (listaValidadeOperadores!!2)],
                                        [validaOperacao operadorADireita valorSelecionado (matrizValores !!linha  !!(coluna + 1))| fst (listaValidadeOperadores!!3)]]
        
        isValido = not (False `elem` listaValidadeOperacao)

    in isValido

isOperadorValido :: Operador -> (Bool, Operador)
isOperadorValido operador = 
    case operador of
        'v' -> (True, operador)
        '^' -> (True, operador)
        '>' -> (True, operador)
        '<' -> (True, operador)
        _ -> (False, operador)

validaOperacao :: Operador -> Int -> Int -> Bool
validaOperacao operador valorA valorB =
    if valorA == 0 || valorB == 0 then
        True
    else if operador == '>' || operador == '^' then
        valorA > valorB
    else 
        valorA < valorB
