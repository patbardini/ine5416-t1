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
