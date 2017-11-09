#include <stdio.h>
#include "source.h"

/* Task is from Aalto C programming Basics Course
 * Objective: formatted input and output, conditional statements
 * Write function calculate() that reads three values from the user, floating point number, character and 
 * floating point number.
 * Character should be one of the following operators: '+', '-', '*' or '/'. 
 * If the user doesn't give a valid number-operator-number input, the function should print "error" to stdout.
 * If the input is correct, the function should perform the given calculation and print it in the following format:
 * 2.00 * 3.00 = 6.00
 * or
 * 0.10 + 0.20 = 0.30
 * Print two decimals in every number and add a newline in the end. */

int main() {
    calculate();
    return 0;
}
