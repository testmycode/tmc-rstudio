#include <check.h>
#include "tmc-check.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "../src/source.h"


START_TEST(test_average) {
    FILE *mock_input = freopen("mockinput", "w+", stdin);
     fputs("1 2 3 4 5 -1", mock_input);
     fseek(mock_input, 0, SEEK_SET);
     freopen("mockoutput", "w", stdout);
     calculate_average();
     fflush(stdout);
     FILE *fp = fopen("mockoutput", "r");
     char userString[100];
     memset(userString, 0, sizeof(userString));
     fgets(userString, 100, fp);
     char *rightString = "3.00\n";
     char message[100] = "";
     int ret = mycompare(userString, rightString, message);
     fail_unless(!ret, "When giving input \"1 2 3 4 5 -1\", your output:\n%s\nReference output:\n%s\nReason: %s\n",
            userString, rightString, message);
     fclose(fp);
}
END_TEST

int main(int argc, const char *argv[])
{
    srand((unsigned)time(NULL));
	Suite *s = suite_create("calculateAverage");
	tmc_register_test(s, test_average, "calculateAverage");
	return tmc_run_tests(argc, argv, s);
}
