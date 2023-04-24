module Resolvedor where

import Matriz

type Posicao = (Int, Int)

getOperadorAcima :: MatrizOperadores -> Posicao -> Operador
getOperadorAcima matriz (x, y) = 
    if (x > 0) then
        if ((matriz!!(x-1))!!y /= 'x') then
            (matriz!!(x-1))!!y
        else
            '|'
    else
        '|'

getOperadorAbaixo :: MatrizOperadores -> Posicao -> Operador
getOperadorAbaixo matriz (x, y) = 
    if (x+1 < length matriz) then
        if ((matriz!!(x+1))!!y /= 'x') then
            (matriz!!(x+1))!!y
        else
            '|'
    else
        '|'

getOperadorAEsquerda :: MatrizOperadores -> Posicao -> Operador
getOperadorAEsquerda matriz (x, y) = 
    if (y > 0) then
        if ((matriz!!x)!!(y-1) /= 'x') then
            (matriz!!x)!!(y-1)
        else
            '|'
    else
        '|'

getOperadorADireita :: MatrizOperadores -> Posicao -> Operador
getOperadorADireita matriz (x, y) = 
    if (y+1 < length matriz) then
        if ((matriz!!x)!!(y+1) /= 'x') then
            (matriz!!x)!!(y+1)
        else
            '|'
    else
        '|'

ehMaiorQueTodosVizinhos :: MatrizOperadores -> Posicao -> Bool
ehMaiorQueTodosVizinhos matriz (x, y) = do
    let operadorAcima = getOperadorAcima matriz (x, y)
    let operadorAbaixo = getOperadorAbaixo matriz (x, y)
    let operadorAEsquerda = getOperadorAEsquerda matriz (x, y)
    let operadorADireita = getOperadorADireita matriz (x, y)
    let elemento = matriz!!x!!y

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
ehMenorQueTodosVizinhos matriz (x, y) = do
    let operadorAcima = getOperadorAcima matriz (x, y)
    let operadorAbaixo = getOperadorAbaixo matriz (x, y)
    let operadorAEsquerda = getOperadorAEsquerda matriz (x, y)
    let operadorADireita = getOperadorADireita matriz (x, y)
    let elemento = matriz!!x!!y

    if (elemento /= 'x') then
        False
    else if ((operadorAcima == 'v' || operadorAcima == '|') 
          && (operadorAbaixo == '^' || operadorAbaixo == '|')
          && (operadorAEsquerda == '>' || operadorAEsquerda == '|')
          && (operadorADireita == '<' || operadorADireita == '|')) then
            True
    else
        False
