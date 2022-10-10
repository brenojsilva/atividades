#include <stdio.h>
#include <string.h>

#define N 100

int main() {
    char nome[N], pequena[N], grande[N];
    float altura, menor = 1000, maior = 0;
    char fim[N];

    do {
        fgets(nome, N, stdin);
        nome[strcspn(nome, "\n")] = 0;

        scanf("%f ", &altura);

        if(altura < menor) {
            menor = altura;
            strcpy(pequena, nome);
        }
        
        if(altura > maior) {
            maior = altura;
            strcpy(grande, nome);
        } 

        fgets(fim, N, stdin);
        fim[strcspn(fim, "\n")] = 0;
        stringToUpper(fim);

    } while(strcmp("FIM", fim) != 0);

    stringToUpper(pequena);
    stringToUpper(grande);

    printf("%s\n", pequena);
    printf("%s", grande);
}

void stringToUpper(char *string) {
    for (int i = 0; string[i] != 0; i++)
        string[i] = toupper(string[i]);
}