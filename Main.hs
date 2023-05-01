import Matriz
import Resolvedor
import Backtracking

main = do
    
    let matrizValores = criaMatrizValores 6
    let matrizOperadores = matriz6x6

    let (a, b) = backtrancking matriz6x6 matrizValores (0, 0) 6

    print a
    print b

