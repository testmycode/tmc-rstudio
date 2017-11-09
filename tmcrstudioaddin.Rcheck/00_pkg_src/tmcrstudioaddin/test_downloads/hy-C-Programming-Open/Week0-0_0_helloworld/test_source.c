#include <check.h>
#include "tmc-check.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "../src/source.h"


START_TEST(test_hello) {
    FILE* fp;
    fp = freopen("mockoutput", "w", stdout);
    hello_world();
    fclose(fp);
    fp = fopen("mockoutput", "r");
    char userString[100];
    char message[100] = "";
    char* rightString = "Hello World!\n";
    fread(userString, 99, 1, fp);
    int ret = mycompare(userString, rightString, message);
    ck_assert_msg(!ret, "Your output:\n%s\nRight output:\n%s\n%s\n", userString, rightString, message);
    fclose(fp);
}
END_TEST

int main(int argc, const char *argv[]) {
    srand((unsigned)time(NULL));
	Suite *s = suite_create("hello-world");
	tmc_register_test(s, test_hello, "hello");
	return tmc_run_tests(argc, argv, s);
}
