module Resolvedor where

import Matriz

type Posicao = (Int, Int)

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


getPosicaoNumeroValido :: MatrizOperadores -> MatrizValores -> Posicao -> Int -> Bool
getPosicaoNumeroValido matrizOperadores matrizValores (linha, coluna) valorSelecionado  = do
    -- validações de maior/menor
    let operadorAcima = getOperadorAcima matrizOperadores (linha, coluna)
    let operadorAbaixo = getOperadorAbaixo matrizOperadores (linha, coluna)
    let operadorAEsquerda = getOperadorAEsquerda matrizOperadores (linha, coluna)
    let operadorADireita = getOperadorADireita matrizOperadores (linha, coluna)
    let tamanhoMatriz = getDimensaoMatriz matrizOperadores
    
    if operadorAcima /= '|' &&  not (validaOperacao operadorAcima valorSelecionado (matrizValores !!(linha - 1) !!coluna)) then
        False 
    else if operadorAbaixo /= '|' &&  not (validaOperacao operadorAbaixo (matrizValores !!(linha + 1) !!coluna) valorSelecionado) then
        False
    else if operadorAEsquerda /= '|' && not (validaOperacao operadorAEsquerda (matrizValores !!linha !!(coluna - 1)) valorSelecionado) then
        False
    else if operadorADireita /= '|' && not (validaOperacao operadorADireita valorSelecionado (matrizValores !!linha  !!(coluna + 1))) then
        False
    else
        -- validar repetição linha/coluna
        if (isRepetidoValorLinha matrizValores valorSelecionado (linha, 0) tamanhoMatriz) then
            False
        else if (isRepetidoValorColuna matrizValores valorSelecionado (0, coluna) tamanhoMatriz) then
            False
        else
            True 

isRepetidoValorLinha :: MatrizValores -> Int -> (Int, Int) -> Int -> Bool
isRepetidoValorLinha matrizValores valorSelecionado (linha, coluna) qtdColunas =

    if coluna == (qtdColunas - 1) then
        False
    else
        isRepetidoValorLinha matrizValores valorSelecionado (linha, (coluna + 1)) qtdColunas

isRepetidoValorColuna :: MatrizValores -> Int -> (Int, Int) -> Int -> Bool
isRepetidoValorColuna matrizValores valorSelecionado (linha, coluna) qtdLinhas = 

    if linha == (qtdLinhas - 1) then
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

 
    





