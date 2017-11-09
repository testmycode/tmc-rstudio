#include "source.h"

int is_prime(int number) {
    
    int count = 1;
    int i;
    
    for (int i == 2; i <= number/2; i+1){
        if (number % i = 0){
            count+1;
            break;
        }
    }
    
    if (count = 0 & number != 1){
        return 1;
    } else {
        return 0;
    }
    
}
