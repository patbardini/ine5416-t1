module Matriz where

type Operador = Char
type MatrizOperadores = [[Operador]]
type MatrizValores = [[Int]]

matriz4x4 :: MatrizOperadores
-- exemplo de matriz 4x4
-- https://www.janko.at/Raetsel/Sudoku/Vergleich/001.a.htm
matriz4x4 = [['x', '<', 'x',     'x', '<', 'x'],
             ['v', '|', '^',     '^', '|', 'v'],
             ['x', '<', 'x',     'x', '>', 'x'],

             ['x', '>', 'x',     'x', '<', 'x'],
             ['v', '|', '^',     '^', '|', 'v'],
             ['x', '>', 'x',     'x', '>', 'x']]

matrizResultado :: MatrizValores
matrizResultado = [[2, 0,    0, 4],
                   [0, 4,    3, 2],

                   [4, 3,    2, 3],
                   [3, 2,    0, 1]]

matriz6x6 :: MatrizOperadores
-- exemplo de matriz 6x6
-- https://www.janko.at/Raetsel/Sudoku/Vergleich/010.a.htm
matriz6x6 = [['x', '>', 'x',     'x', '>', 'x',     'x', '<', 'x'],
             ['v', '|', 'v',     'v', '|', '^',     '^', '|', 'v'],
             ['x', '>', 'x',     'x', '<', 'x',     'x', '>', 'x'],
             ['v', '|', '^',     '^', '|', 'v',     'v', '|', 'v'],
             ['x', '<', 'x',     'x', '>', 'x',     'x', '>', 'x'],

             ['x', '<', 'x',     'x', '>', 'x',     'x', '<', 'x'],
             ['^', '|', 'v',     'v', '|', 'v',     '^', '|', '^'],
             ['x', '>', 'x',     'x', '<', 'x',     'x', '<', 'x'],
             ['v', '|', '^',     '^', '|', 'v',     '^', '|', 'v'],
             ['x', '<', 'x',     'x', '>', 'x',     'x', '>', 'x']]

matriz9x9 :: MatrizOperadores
-- exemplo de matriz 9x9
-- https://www.janko.at/Raetsel/Sudoku/Vergleich/190.a.htm
matriz9x9 = [['x', '<', 'x', '>', 'x',      'x', '<', 'x', '<', 'x',      'x', '>', 'x', '>', 'x'],
             ['^', '|', 'v', '|', 'v',      'v', '|', '^', '|', '^',      'v', '|', '^', '|', '^'],
             ['x', '>', 'x', '<', 'x',      'x', '<', 'x', '<', 'x',      'x', '>', 'x', '<', 'x'],
             ['v', '|', '^', '|', '^',      '^', '|', 'v', '|', 'v',      '^', '|', '^', '|', '^'],
             ['x', '>', 'x', '<', 'x',      'x', '>', 'x', '<', 'x',      'x', '>', 'x', '<', 'x'],

             ['x', '<', 'x', '>', 'x',      'x', '>', 'x', '<', 'x',      'x', '<', 'x', '>', 'x'],
             ['v', '|', '^', '|', '^',      'v', '|', 'v', '|', 'v',      '^', '|', 'v', '|', '^'],
             ['x', '<', 'x', '>', 'x',      'x', '>', 'x', '<', 'x',      'x', '<', 'x', '>', 'x'],
             ['^', '|', 'v', '|', 'v',      '^', '|', '^', '|', 'v',      'v', '|', 'v', '|', '^'],
             ['x', '<', 'x', '>', 'x',      'x', '>', 'x', '>', 'x',      'x', '>', 'x', '<', 'x'],

             ['x', '>', 'x', '<', 'x',      'x', '<', 'x', '<', 'x',      'x', '>', 'x', '<', 'x'],
             ['^', '|', 'v', '|', 'v',      '^', '|', '^', '|', 'v',      '^', '|', '^', '|', 'v'],
             ['x', '>', 'x', '>', 'x',      'x', '>', 'x', '>', 'x',      'x', '<', 'x', '>', 'x'],
             ['v', '|', 'v', '|', '^',      'v', '|', '^', '|', '^',      '^', '|', '^', '|', '^'],
             ['x', '>', 'x', '>', 'x',      'x', '<', 'x', '>', 'x',      'x', '<', 'x', '<', 'x']]

getDimensaoMatriz :: MatrizOperadores -> Int
getDimensaoMatriz (m:ms) = length $ filter (== 'x') m

getDimensaoRegiao :: Int -> (Int, Int)
getDimensaoRegiao 4 = (2, 2)
getDimensaoRegiao 6 = (2, 3)
getDimensaoRegiao 9 = (3, 3)

-- cria uma matriz quadrada preenchida com zeros com o tamanho passado como parâmetro
criaMatrizValores :: Int -> MatrizValores
criaMatrizValores dimensao = replicate dimensao . replicate dimensao $ 0
