import Matriz
import Resolvedor
import Backtracking

main = do
    
    let matrizValores = criaMatrizValores 9
    let matrizOperadores = matriz9x9
    let matriz = matrizResultado 9

    let (a, b) = backTracking matrizOperadores matrizValores (0, 0) 1 (1, (length matrizValores))
    putStrLn (gridToString b)


