#include <check.h>
#include "tmc-check.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "../src/source.h"


START_TEST(test_binary_to_decimal)
{
    ck_assert_msg(binary_to_decimal(1110) == 14, "Binary number 1110 should return 14, but it returned %d\n", binary_to_decimal(1110));
    ck_assert_msg(binary_to_decimal(11101110) == 238, "Binary number 1110110 should return 238, but it returned %d\n", binary_to_decimal(11101110));
}
END_TEST

int main(int argc, const char *argv[])
{
    srand((unsigned)time(NULL));
	Suite *s = suite_create("binary2decimal");
	tmc_register_test(s, test_binary_to_decimal, "binary2decimal");
	return tmc_run_tests(argc, argv, s);
}
