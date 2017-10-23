#include <check.h>
#include "tmc-check.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "../src/source.h"


START_TEST(test_calculate) {
    FILE *mock_input = freopen("mockinput", "w+", stdin);
     fputs("5.5 + 6.5", mock_input);
     fseek(mock_input, 0, SEEK_SET);
     freopen("mockoutput", "w", stdout);
     calculate();
     fflush(stdout);
     FILE *fp = fopen("mockoutput", "r");
     char str [100];
     memset(str, 0, sizeof(str));
     fgets(str, 100, fp);
     char *ref = "5.50 + 6.50 = 12.00\n";
     char infostr[100] = "";
     int ret = mycompare(str, ref, infostr);
     fail_unless(!ret, "When giving input \"5.5 + 6.5\", your output:\n%s\nReference output:\n%s\nReason: %s\n",
            str, ref, infostr);
     fclose(fp);
     
     mock_input = freopen("mockinput", "w+", stdin);
     fputs("5.1 - 0.2", mock_input);
     fseek(mock_input, 0, SEEK_SET);
     freopen("mockoutput", "w", stdout);
     calculate();
     fflush(stdout);
     fp = fopen("mockoutput", "r");
     memset(str, 0, sizeof(str));
     fgets(str, 100, fp);
     ref = "5.10 - 0.20 = 4.90\n";
     infostr[0] = 0;
     ret = mycompare(str, ref, infostr);
     fail_unless(!ret, "When giving input \"5.1 - 0.2\", your output:\n%s\nReference output:\n%s\nReason: %s\n",
            str, ref, infostr);
     fclose(fp);
     
     mock_input = freopen("mockinput", "w+", stdin);
     fputs("5.1 a 0.2", mock_input);
     fseek(mock_input, 0, SEEK_SET);
     freopen("mockoutput", "w", stdout);
     calculate();
     fflush(stdout);
     fp = fopen("mockoutput", "r");
     memset(str, 0, sizeof(str));
     fgets(str, 100, fp);
     ref = "error\n";
     infostr[0] = 0;
     ret = mycompare(str, ref, infostr);
     fail_unless(!ret, "When giving input \"5.1 a 0.2\", your output:\n%s\nReference output:\n%s\nReason: %s\n",
            str, ref, infostr);
     fclose(fp);
     
     mock_input = freopen("mockinput", "w+", stdin);
     fputs("4.2 * 2", mock_input);
     fseek(mock_input, 0, SEEK_SET);
     freopen("mockoutput", "w", stdout);
     calculate();
     fflush(stdout);
     fp = fopen("mockoutput", "r");
     memset(str, 0, sizeof(str));
     fgets(str, 100, fp);
     ref = "4.20 * 2.00 = 8.40\n";
     infostr[0] = 0;
     ret = mycompare(str, ref, infostr);
     fail_unless(!ret, "When giving input \"4.2 * 2\", your output:\n%s\nReference output:\n%s\nReason: %s\n",
            str, ref, infostr);
     fclose(fp);
     
     mock_input = freopen("mockinput", "w+", stdin);
     fputs("4.2 / 2", mock_input);
     fseek(mock_input, 0, SEEK_SET);
     freopen("mockoutput", "w", stdout);
     calculate();
     fflush(stdout);
     fp = fopen("mockoutput", "r");
     memset(str, 0, sizeof(str));
     fgets(str, 100, fp);
     ref = "4.20 / 2.00 = 2.10\n";
     infostr[0] = 0;
     ret = mycompare(str, ref, infostr);
     fail_unless(!ret, "When giving input \"4.2 / 2\", your output:\n%s\nReference output:\n%s\nReason: %s\n",
            str, ref, infostr);
     fclose(fp);
     
     mock_input = freopen("mockinput", "w+", stdin);
     fputs("a / b", mock_input);
     fseek(mock_input, 0, SEEK_SET);
     freopen("mockoutput", "w", stdout);
     calculate();
     fflush(stdout);
     fp = fopen("mockoutput", "r");
     memset(str, 0, sizeof(str));
     fgets(str, 100, fp);
     ref = "error\n";
     infostr[0] = 0;
     ret = mycompare(str, ref, infostr);
     fail_unless(!ret, "When giving input \"a / b\", your output:\n%s\nReference output:\n%s\nReason: %s\n",
            str, ref, infostr);
     fclose(fp);
}
END_TEST


int main(int argc, const char *argv[])
{
    srand((unsigned)time(NULL));
	Suite *s = suite_create("Calculator");

	tmc_register_test(s, test_calculate, "calculator");
        
	return tmc_run_tests(argc, argv, s);
}
