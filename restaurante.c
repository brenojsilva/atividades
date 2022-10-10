#include <stdio.h>
#include <string.h>

#define MAX 50

int main() {
    char tipo[MAX], sabor[MAX];
    float quantidade, valor, frete;

    fgets(tipo, MAX, stdin);
    tipo[strcspn(tipo, "\n")] = 0;
    for (int i = 0; tipo[i] != '\0'; i++)
        tipo[i] = tolower(tipo[i]);

    fgets(sabor, MAX, stdin);
    sabor[strcspn(sabor, "\n")] = 0;
    for (int i = 0; sabor[i] != '\0'; i++)
        sabor[i] = tolower(sabor[i]);

    scanf("%f", &quantidade);
    frete = quantidade * 2;

    if (strcmp("pizza", tipo) == 0) {
        if (strcmp("calabresa", sabor) == 0 || strcmp("marguerita", sabor) == 0)
            valor = (quantidade * 20.60) + frete;
        else
            valor = (quantidade * 30.90) + frete;
    }
    else if (strcmp("lasanha", tipo) == 0) {
        if(strcmp("queijo", sabor) == 0)
            valor = (quantidade * 18) + frete;

        else if(strcmp("bolonhesa", sabor) == 0)
            valor = (quantidade * 23) + frete;
    }
    printf("%.2f", valor);
}