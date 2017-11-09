#include <stdio.h>
#include <stdlib.h>
#include "source.h"

/* Correct function int is_prime(int number). It should return 1 if number is a prime number 
 * and 0 if it is not. */

int main() {
    int number = 7;
    int boolean = is_prime(number);
    if (boolean == 0){
        printf("%d is not a prime number\n", number);
    } else {
        printf("%d is a prime number\n", number);
    }
    return 0;
}
