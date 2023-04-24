import Matriz
import Resolvedor

main = do
    let dimensaoMatriz = getDimensaoMatriz matriz4x4
    let matrizValores = criaMatrizValores dimensaoMatriz
    print(getPosicaoNumeroValido matriz4x4 matrizValores (0, 0) 2) 
    
    print(getOperadorAbaixo matriz4x4 (0, 0))
    print(getOperadorAcima matriz4x4 (0, 0))
    print(getOperadorAEsquerda matriz4x4 (0, 0))
    print(getOperadorADireita matriz4x4 (0, 0))
