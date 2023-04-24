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

getPosicaoNumeroValido :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Bool
getPosicaoNumeroValido matrizOperadores matrizValores (linha, coluna) valorSelecionado  = do
    -- validações de maior/menor
    let posicaoNaMatrizOperadores = getPosicaoEquivalenteMatrizOperadores matrizOperadores (linha, coluna)
    let linhaMatrizOperadores = fst posicaoNaMatrizOperadores
    let colunaMatrizOperadores = snd posicaoNaMatrizOperadores

    let operadorAcima = getOperadorAcima matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)
    let operadorAbaixo = getOperadorAbaixo matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)
    let operadorAEsquerda = getOperadorAEsquerda matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)
    let operadorADireita = getOperadorADireita matrizOperadores (linhaMatrizOperadores, colunaMatrizOperadores)

    let tamanhoMatriz = getDimensaoMatriz matrizOperadores

    if (isRepetidoValorLinha matrizValores valorSelecionado (linha, 0) tamanhoMatriz) then
            False
        else if (isRepetidoValorColuna matrizValores valorSelecionado (0, coluna) tamanhoMatriz) then
            False
        else
            if operadorAcima /= '|' &&  not (validaOperacao operadorAcima valorSelecionado (matrizValores !!(linha - 1) !!coluna)) then
                False 
            else if operadorAbaixo /= '|' &&  not (validaOperacao operadorAbaixo (matrizValores !!(linha + 1) !!coluna) valorSelecionado) then
                False
            else if operadorAEsquerda /= '|' && not (validaOperacao operadorAEsquerda (matrizValores !!linha !!(coluna - 1)) valorSelecionado) then
                False
            else if operadorADireita /= '|' && not (validaOperacao operadorADireita valorSelecionado (matrizValores !!linha  !!(coluna + 1))) then
                False
            else 
                True

isRepetidoValorLinha :: MatrizValores -> Int -> (Int, Int) -> Int -> Bool
isRepetidoValorLinha matrizValores valorSelecionado (linha, coluna) qtdColunas =

    if (matrizValores !!linha !!coluna) == valorSelecionado then
        True
    else if coluna == (qtdColunas - 1) then
        False
    else
        isRepetidoValorLinha matrizValores valorSelecionado (linha, (coluna + 1)) qtdColunas

isRepetidoValorColuna :: MatrizValores -> Int -> (Int, Int) -> Int -> Bool
isRepetidoValorColuna matrizValores valorSelecionado (linha, coluna) qtdLinhas = 

    if (matrizValores !!linha !!coluna) == valorSelecionado then
        True
    else if linha == (qtdLinhas - 1) then
        False
    else
        isRepetidoValorColuna matrizValores valorSelecionado ((linha + 1), coluna) qtdLinhas

validaOperacao :: Char -> Int -> Int -> Bool
validaOperacao operador valorA valorB =
    if valorA == 0 || valorB == 0 then
        True
    else if operador == '>' || operador == '^' then
        valorA > valorB
    else 
        valorA < valorB
