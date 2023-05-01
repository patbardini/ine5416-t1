import Matriz
import Resolvedor
import Backtracking

main = do
    -- let dimensaoMatriz = getDimensaoMatriz matriz4x4

    let matrizValores = matrizResultado
    -- print(inserirValorMatriz matrizValores (1, 1) 1)

    let (a, b) = backtrancking matriz4x4 matrizValores (0, 0) 4
    print a
    print b

    --print(getPosicaoNumeroValido matriz4x4 matrizValores (1, 3) 1) 
    
    -- print(getOperadorAbaixo matriz4x4 (0, 0))
    -- print(getOperadorAcima matriz4x4 (0, 0))
    -- print(getOperadorAEsquerda matriz4x4 (0, 0))
    -- print(getOperadorADireita matriz4x4 (0, 0))
    
    let matrizValores = matrizResultado 9
    let matrizOperadores = matriz9x9
    let tamanhoMatriz = getDimensaoMatriz matrizOperadores

    print(getPosicaoNumeroValido matrizOperadores matrizValores (7, 8) 7)

