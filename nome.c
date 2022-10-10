#include <stdio.h>
#include <string.h>

int main(){
    char nome[10];
    int i, tamanho;
    fgets(nome,10,stdin);
    tamanho = strlen(nome); 

    for(i = tamanho-1; i >= 0; i--){
        if(i == tamanho-1 || i == 0){
            printf("%c", toupper(nome[i]));
        }
        else{ 
        printf("%c", tolower(nome[i]));
        }
    }
    return 0;
}