#include <check.h>
#include "tmc-check.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "../src/source.h"


START_TEST(test_prime)
{
    fail_unless(is_prime(5) == 1, "is_prime(5) should return 1, because 5 is a prime number");
    fail_unless(is_prime(4) == 0, "is_prime(4) should return 0, because 4 is not a prime number");
}
END_TEST

int main(int argc, const char *argv[])
{
    srand((unsigned)time(NULL));
	Suite *s = suite_create("prime");
	tmc_register_test(s, test_prime, "prime");
	return tmc_run_tests(argc, argv, s);
}
