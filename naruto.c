#include <stdio.h>

int main(){
    char frase[100];
    int i;
    gets(frase);
    
    for(i = 0; frase[i] != 0; i++){
        if (frase[i] >= 'a' && frase[i] <= 'z') {
            frase[i] = 219 - frase[i];
        }
    }
    printf("%s", frase);
}