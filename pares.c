#include <stdio.h>

int main(){
    int N, x, qtd_pares, aux;
    scanf("%i", &N);
    int numeros[N];

    for (int i = 0; i < N; i++){    
        scanf(" %i", &x);
        numeros[i] = x;
    }

    for (int i = 0; i < N; i++){
        if(numeros[i] % 2 == 0){
            qtd_pares += 1;
        }
    }

    int qtd_impares = N - qtd_pares;
    int pares[qtd_pares], impares[qtd_impares];
    int par_index = 0, impar_index = 0;

    for (int i = 0; i < N; i++){
        if(numeros[i] % 2 == 0){
            pares[par_index++] = numeros[i];
        }
        else{
            impares[impar_index++] = numeros[i];
        }
    }

    for (int i = 0; i < qtd_pares; i++) {
        for (int j = i + 1; j < qtd_pares; j++) {
            if (pares[i] > pares[j]) {
                aux = pares[i];
                pares[i] = pares[j];
                pares[j] = aux;
            }
        }
    }

    for (int i = 0; i < qtd_impares; i++){
        for(int j = i + 1; j < qtd_impares; j++){
        if (impares[i] < impares[j]){
            aux = impares[i];
            impares[i] = impares[j];
            impares[j] = aux;
            }
        }
    }

    for (int i = 0; i < qtd_pares; i++) {
        printf("%i\n", pares[i]);
    }

    for (int i = 0; i < qtd_impares; i++) {
        printf("%i\n", impares[i]);
    }

}


