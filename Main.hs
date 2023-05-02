import Matriz
import Resolvedor
import Backtracking

main = do
    
    let matrizValores = criaMatrizValores 6
    let matrizOperadores = matriz6x6
    
    -- print (ehMenorQueTodosVizinhos matrizOperadores (getPosicaoEquivalenteMatrizOperadores matrizOperadores (5,5)) )
    let (a, b) = backTrackingMenor matriz6x6 matrizValores (0, 0) 1
    print a
    print b


