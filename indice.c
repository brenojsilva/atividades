#include <stdio.h>
#include <string.h>

#define MAX 50

int main() {
    char string[MAX], carac;

    fgets(string, MAX, stdin);
    scanf("%c", &carac);

    for (int i = 0; string[i] != '\0' ; i++)
            if(string[i] == carac){
            printf("%d\n", i);
        }
    printf("-1");
}