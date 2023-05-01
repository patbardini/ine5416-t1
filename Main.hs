import Matriz
import Resolvedor

main = do
    let matrizValores = matrizResultado 9
    let matrizOperadores = matriz9x9
    let tamanhoMatriz = getDimensaoMatriz matrizOperadores

    print(getPosicaoNumeroValido matrizOperadores matrizValores (7, 8) 7)

