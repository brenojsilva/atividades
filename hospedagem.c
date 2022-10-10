#include <stdio.h>
#include <string.h>

float CalculaHospedagem(char *tipo, int dias);

int main(){
    char x[50];
    int y;
    float resultado;

    fgets(x, 50, stdin);
    x[strcspn(x, "\n")] = 0;
    for (int i = 0; x[i] != '\0'; i++)
        x[i] = tolower(x[i]);

    scanf("%i", &y);
    resultado = CalculaHospedagem(x, y);
    printf("%.2f\n", resultado);
    return 0;
}

float CalculaHospedagem(char *tipo, int dias){
    float resultado;
    if(strcmp("individual", tipo) == 0){
        if(dias >= 3){
           resultado = dias * 106.25;
        }
        else{
            resultado = dias * 125;
        } 
    }
    else if(strcmp("suíte dupla", tipo) == 0){
        if(dias >= 3){
           resultado = dias * 119;
        }
        else{
            resultado = dias * 140;
        }
    }
    else if(strcmp("suíte tripla", tipo) == 0){
        if(dias >= 3){
           resultado = dias * 153;
        }
        else{
            resultado = dias * 180;
        }
    }
    return resultado;
}