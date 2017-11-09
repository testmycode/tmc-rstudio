#include <check.h>
#include "tmc-check.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../src/mathfuncs.h"


START_TEST(test_sum_of_absolutes)
{
	fail_unless(sum_of_absolutes(3, -2) == 5, "sum_of_absolutes(3, -2) should return 5");
	fail_unless(sum_of_absolutes(-8, 0.5) == 8.5, "sum_of_absolutes(-8, 0.5) should return 8.5");
	fail_unless(sum_of_absolutes(10, 0.5) == 10.5, "sum_of_absolutes(10, 0.5) should return 10.5");
	fail_unless(sum_of_absolutes(-1, -9) == 10, "sum_of_absolutes(-1, -9) should return 10");
}
END_TEST

START_TEST(test_sum_of_rounded)
{
	fail_unless(sum_of_rounded(0, 0) == 0, "sum_of_rounded(0, 0) should return 0");
	fail_unless(sum_of_rounded(0.2, 0.1) == 2, "sum_of_rounded(0.2, 0.1) should return 2");
	fail_unless(sum_of_rounded(0.1, 0) == 1, "sum_of_rounded(0.1, 0) should return 1");
	fail_unless(sum_of_rounded(0.9, 1.8) == 3, "sum_of_rounded(0.9, 1.8) should return 3");
	fail_unless(sum_of_rounded(-0.9, 1) == 1, "sum_of_rounded(-0.9, 1) should return 1");
}
END_TEST

START_TEST(test_sum_of_characters)
{
	fail_unless(sum_of_characters('a', 'x') == 217, "sum_of_characters('a', 'x') should return 217");
	fail_unless(sum_of_characters('1', '0') == 97, "sum_of_characters('1', '0') should return 97");
	fail_unless(sum_of_characters('b', 'b') == 196, "sum_of_characters('a', 'x') should return 196");
	fail_unless(sum_of_characters(100, 1) == 'e', "sum_of_characters(100, 1) should return 'e'");
}
END_TEST

int main(int argc, const char *argv[])
{
	Suite *s = suite_create("Test");

	tmc_register_test(s, test_sum_of_absolutes, "sum_abs");
	tmc_register_test(s, test_sum_of_rounded, "sum_round");
	tmc_register_test(s, test_sum_of_characters, "sum_char");

	return tmc_run_tests(argc, argv, s);
}
