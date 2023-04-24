module Matriz where

type Operador = Char
type MatrizOperadores = [[Operador]]
type Matriz = [[Int]]

matriz4x4 :: MatrizOperadores
matriz4x4 = [['x', '<', 'x',     'x', '<', 'x'],
             ['v', '|', '^',     '^', '|', 'v'],
             ['x', '<', 'x',     'x', '>', 'x'],

             ['x', '>', 'x',     'x', '<', 'x'],
             ['v', '|', '^',     '^', '|', 'v'],
             ['x', '>', 'x',     'x', '>', 'x']]

getDimensaoMatriz :: MatrizOperadores -> Int
getDimensaoMatriz (m:ms) = length $ filter (== 'x') m

getDimensaoRegiao :: Int -> (Int, Int)
getDimensaoRegiao 4 = (2, 2)
getDimensaoRegiao 6 = (2, 3)
getDimensaoRegiao 9 = (3, 3)

-- cria uma matriz quadrada preenchida com zeros com o tamanho passado como parâmetro
criaMatrizValores :: Int -> Matriz
criaMatrizValores dimensao = replicate dimensao . replicate dimensao $ 0
