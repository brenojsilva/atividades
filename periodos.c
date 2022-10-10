#include <stdio.h>

int main(){
    int ano;
    char periodo;
    scanf(" %i", &ano);
    scanf(" %c%c", &periodo);
    if(ano >= 4000 && periodo == 'a'){
        printf("PRE-HISTORIA");
    }
    else if(ano <= 4001 && periodo == 'a' || ano <= 476 && periodo == 'd'){
        printf("ANTIGUIDADE");
    }
    else if(ano <= 1453 && periodo == 'd'){
        printf("IDADE MEDIA");
        }
    else if(ano <= 1789 && periodo == 'd'){
        printf("IDADE MODERNA");
        } 
    else if(ano >= 1790 && periodo == 'd'){
        printf("IDADE CONTEMPORANEA");
        }
}