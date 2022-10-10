#include <stdio.h>
#include <math.h>

int main(void) {
  int length = 0, aux, vetor[100];
  float mediana;

  while (scanf("%d ", &vetor[length]) != EOF)
    length++;

  for (int i = 0; i < length; i++)
    for (int j = i + 1; j < length; j++)
      if (vetor[i] > vetor[j]) {
        aux = vetor[i];
        vetor[i] = vetor[j];
        vetor[j] = aux;
      }

  if(length % 2)
    mediana = vetor[length / 2];
  else
    mediana = (vetor[(int) floor(length / 2.0)] + vetor[(int) ceil(length / 2.0)]) / 2;

  printf("Mediana = %d", mediana);
}