#include <stdio.h>
#include <string.h>

#define MAX 50

int main() {
    char sabor[MAX];
    float quantidade, valor;

    fgets(sabor, MAX, stdin);
    sabor[strcspn(sabor, "\n")] = 0;
    for (int i = 0; sabor[i] != '\0'; i++)
        sabor[i] = tolower(sabor[i]);

    scanf("%f", &quantidade);

    if(strcmp("morango", sabor) == 0 || strcmp("cereja", sabor) == 0){
        valor =  quantidade * 4.50 ;
    }

    else if(strcmp("damasco", sabor) == 0 || strcmp("ciriguela", sabor) == 0){
        valor =  quantidade * 3.80 ;
    }

    else{
        valor = quantidade * 2.75;
    }

    printf("%.2f\n", valor);
    if(quantidade > 2){
        printf("COM CALDA");
    }
    else{
        printf("SEM CALDA");
    }
}