#include <check.h>
#include "tmc-check.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <limits.h>
#include "../src/source.h"

void print_array(char *buf, int *arr, int n) {
    char b[8];
    strcpy(buf, "{ ");
    for (int i = 0; i < n; i++) {
        sprintf(b, "%d ", arr[i]);
        strcat(buf, b);
    }
    strcat(buf, "}");
}

START_TEST(test_smallest_num) {
    int arr[8];
    for (int i = 0; i < 3; i++) {
        int len = 7;
        int mod = INT_MAX;
        for (int j = 0; j < len; j++) {
            arr[j] = rand() % 100;
            if (arr[j] < mod)
                mod = arr[j];
        }
        int stu = smallest_number(arr, len);
        char outbuf[100] = { 0 };
        print_array(outbuf, arr, len + 1);
        fail_unless(stu == mod, "smallest_number() called with array %s. You returned: %d. Should have been: %d.",
                outbuf, stu, mod);
    }
}
END_TEST

int main(int argc, const char *argv[])
{
    srand((unsigned)time(NULL));
	Suite *s = suite_create("arraySmallest");
	tmc_register_test(s, test_smallest_num, "arraySmallest");
	return tmc_run_tests(argc, argv, s);
}
