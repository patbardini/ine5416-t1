import Matriz
import Resolvedor
import Backtracking

main = do
    
    let matrizValores = criaMatrizValores 9
    let matrizOperadores = matriz9x9
    let matriz = matrizResultado 9

    -- print(encontraIntervalo matrizOperadores matriz (1,0) (1,6))

    -- print (ehMenorQueTodosVizinhos matrizOperadores (getPosicaoEquivalenteMatrizOperadores matrizOperadores (5,5)) )
    let (a, b) = backTracking matriz9x9 matrizValores (0, 0) 1 (1,9)
    print a
    print b


