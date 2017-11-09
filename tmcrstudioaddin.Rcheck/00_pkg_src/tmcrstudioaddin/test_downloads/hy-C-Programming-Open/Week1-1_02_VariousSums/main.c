#include "mathfuncs.h"
#include <stdio.h>

int main()
{
/* Write the following functions. You may need some mathematical functions from the math.h.

a) int sum_of_rounded (double x, double y) that returns the sum of the rounded values.
    The variables x and y are rounded up to the nearest integer before the sum is calculated. 
    This means that 1.1 is rounded to 2 and 2.4 to 3

b) double sum_of_absolutes (double x, double y) that returns the sum of the absolute values of x and y.
     NOTE that absolute value of integer -2 is just 2. abs(-2)==abs(2)

c) int sum_of_characters (char a, char b) that returns the sum of two characters. 
    (C is not strongly typed, so you can use various types in arithmetical expressions.)
*/


       printf("Sum_of_rounded 1.1 + 2.4 should be 5, and it is %d\n", sum_of_rounded (1.5, 2.4) );

	printf("Sum of 'a' and 'b' is %d\n", sum_of_characters('a', 'b') );

       printf("Sum of absolute:  -345.67 + 111.1 should be 456.77, and it is %3.2f\n", sum_of_absolutes(-345.67,111.1) );

	return 0;
}
