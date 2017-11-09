#include <stdio.h>
#include "mathfuncs.h"

int main()


/*    Greatest Common Divisor (gcd)
a) Write a recursive function int gcd (int a, int b) that returns the greatest common divisor of 
two positive integers a and b.  Use the Euclidean algorithm:

            1. If a <b, exhange a and b
            2. Divide a by b and get the remainder r.
                If r = 0, report b as the GCD of a and b.
            3. Replace a by b and replace b by r.
                Return to previous step.

b) Write an iterative version of the algorithm as function gcd_iterative.
*/

{

      printf("Greatest common divisor of 64 and 8 is 8, function returned %d\n", gcd (64,8) );
	return 0;
}
