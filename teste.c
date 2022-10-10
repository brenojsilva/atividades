#include <stdio.h>

int main() {
    int x, aux, array[3];

    for (int i = 0; i < 3; i++){
        scanf("%i", &x);
        if (x < 0){
            printf("Ordenacao cancelada.");
            return 0;
        }
        else{
        array[i] = x;
        }
    }
    
    if (array[0] % 2 != 0) {
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (array[i] < array[j]) {
                    aux = array[i];
                    array[i] = array[j];
                    array[j] = aux;
            }
        }
    }
        for(int i = 0; i < 3; i++) {
            printf("%i\n", array[i]);
        }
    }
    else {
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (array[i] < array[j]) {
                    aux = array[i];
                    array[i] = array[j];
                    array[j] = aux;
            }
        }
    }
        for(int i = 2; i >= 0; i--) {
            printf("%i\n", array[i]);
        }

    }

}