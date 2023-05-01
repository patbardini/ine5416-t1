import Matriz
import Resolvedor
import Backtracking

main = do
    
    let matrizValores = criaMatrizValores 6
    let matrizOperadores = matriz6x6

   -- let (a, b) = backtrancking matriz9x9 matrizValores (0, 0) 9
    print(getPosicaoNumeroValido matrizOperadores (matrizResultado 6)(5,5) 5)
   -- print a
  --  print b

