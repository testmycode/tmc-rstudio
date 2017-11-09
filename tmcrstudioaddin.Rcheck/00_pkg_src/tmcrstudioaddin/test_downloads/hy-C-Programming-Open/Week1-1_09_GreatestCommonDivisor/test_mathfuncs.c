#include <check.h>
#include "tmc-check.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../src/mathfuncs.h"


START_TEST(test_gcd)
{
	fail_unless(gcd(5, 2) == 1, "gcd(5, 2) should return 1");
	fail_unless(gcd(16, 24) == 8, "gcd(16, 24) should return 8");
	fail_unless(gcd(99, 66) == 33, "gcd(99, 66) should return 33");
	fail_unless(gcd(15, 20) == 5, "gcd(15, 20) should return 5");
}
END_TEST

START_TEST(test_gcd_iterative)
{
	/*Unfortunately I don't know how we could test whether the solution is actually iterative*/
	fail_unless(gcd_iterative(5, 2) == 1, "gcd_iterative(5, 2) should return 1");
	fail_unless(gcd_iterative(16, 24) == 8, "gcd_iterative(16, 24) should return 8");
	fail_unless(gcd_iterative(99, 66) == 33, "gcd_iterative(99, 66) should return 33");
	fail_unless(gcd_iterative(15, 20) == 5, "gcd_iterative(15, 20) should return 5");
}
END_TEST

int main(int argc, const char *argv[])
{
	Suite *s = suite_create("Test");

	tmc_register_test(s, test_gcd, "gcr_rec");
	tmc_register_test(s, test_gcd_iterative, "gcd_ite");

	return tmc_run_tests(argc, argv, s);
}
