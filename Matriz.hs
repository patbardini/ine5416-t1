module Matriz where

type Operador = Char
type MatrizOperadores = [[Operador]]
type MatrizValores = [[Int]]
type Lista = [Int]

matriz4x4 :: MatrizOperadores
-- exemplo de matriz 4x4
-- https://www.janko.at/Raetsel/Sudoku/Vergleich/001.a.htm
matriz4x4 = [['x', '<', 'x',     'x', '<', 'x'],
             ['v', '|', '^',     '^', '|', 'v'],
             ['x', '<', 'x',     'x', '>', 'x'],

             ['x', '>', 'x',     'x', '<', 'x'],
             ['v', '|', '^',     '^', '|', 'v'],
             ['x', '>', 'x',     'x', '>', 'x']]

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

-- resultados, para testar funcionalidades
matrizResultado :: Int -> MatrizValores
matrizResultado 4 = [[2, 3,    1, 4],
                     [1, 4,    3, 2],

                     [4, 1,    2, 3],
                     [3, 2,    4, 1]]

matrizResultado 6 = [[5, 3,    4, 1,    2, 6],
                     [4, 1,    2, 6,    5, 3],
                     [2, 6,    5, 3,    4, 1],
                     
                     [3, 4,    6, 5,    1, 2],
                     [6, 2,    1, 4,    3, 5],
                     [1, 5,    3, 2,    6, 4]]

matrizResultado 9 = [[3, 7, 6,    4, 5, 8,    9, 2, 1],
                     [8, 1, 5,    2, 6, 9,    7, 3, 4],
                     [4, 2, 9,    7, 1, 3,    8, 5, 6],
                     
                     [5, 8, 4,    6, 3, 7,    1, 9, 2],
                     [1, 9, 7,    5, 2, 4,    6, 8, 3],
                     [2, 6, 3,    9, 8, 1,    5, 4, 7],
                     
                     [7, 5, 8,    3, 4, 6,    2, 1, 9],
                     [9, 4, 1,    8, 7, 2,    3, 6, 5],
                     [6, 3, 2,    1, 9, 5,    4, 7, 8]]

getDimensaoMatriz :: MatrizOperadores -> Int
getDimensaoMatriz (m:ms) = length $ filter (== 'x') m

getDimensaoRegiao :: Int -> (Int, Int)
getDimensaoRegiao 4 = (2, 2)
getDimensaoRegiao 6 = (3, 2)
getDimensaoRegiao 9 = (3, 3)

-- cria uma matriz quadrada preenchida com zeros com o tamanho passado como parâmetro
criaMatrizValores :: Int -> MatrizValores
criaMatrizValores dimensao = replicate dimensao . replicate dimensao $ 0

pedacosDe :: Int -> [(Int, Int)] -> [[(Int, Int)]]
pedacosDe _ [] = []
pedacosDe tamanho xs = take tamanho xs : pedacosDe tamanho (drop tamanho xs)

inserirValorMatriz :: MatrizValores -> (Int, Int) -> Int -> MatrizValores
inserirValorMatriz matrizValores (linha, coluna) numero =
    let 
        linhasAnteriores = take linha matrizValores
        linhasPosteriores = drop (linha+1) matrizValores
        linhaAlterada = alteraElementoLinha (matrizValores !! linha) coluna numero
    in
        linhasAnteriores ++ [linhaAlterada] ++ linhasPosteriores

alteraElementoLinha :: Lista -> Int -> Int -> Lista
alteraElementoLinha lista coluna valor =
    let
        elementosAnterior = take coluna lista
        elementosPosterior = drop (coluna+1) lista
    in
        elementosAnterior ++ [valor] ++ elementosPosterior
