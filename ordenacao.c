#include <stdio.h>

int main(){
    int N, i;
    scanf("%i", &N);
    int vetor[N];

    for (int i = 0; i < N; i++){
        scanf(" %i", &vetor[i]);
    }

    int aux;
    for (int i = 0; i < N; i++) {
        for (int j = i + 1; j < N; j++) {
            if (vetor[i] > vetor[j]) {
                aux = vetor[i];
                vetor[i] = vetor[j];
                vetor[j] = aux;
            }
        }
    }

    for (int i = 0; i < N; i++) {
        printf("%i ", vetor[i]);
    }
}