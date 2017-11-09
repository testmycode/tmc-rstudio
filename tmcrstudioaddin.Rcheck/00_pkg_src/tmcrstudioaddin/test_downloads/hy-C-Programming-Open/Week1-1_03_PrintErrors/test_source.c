#include <check.h>
#include "tmc-check.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "../src/source.h"


START_TEST(test_count_sum) {
    FILE *mock_input = freopen("mockinput", "w+", stdin);
     fputs("6 8", mock_input);
     fseek(mock_input, 0, SEEK_SET);
     freopen("mockoutput", "w", stdout);
     count_sum();
     fflush(stdout);
     FILE *fp = fopen("mockoutput", "r");
     char str [100];
     memset(str, 0, sizeof(str));
     fgets(str, 100, fp);
     char *ref = "6 + 8 = 14\n";
     char infostr[100] = "";
     int ret = mycompare(str, ref, infostr);
     fail_unless(!ret, "When giving input \"6 8\", your output:\n%s\nReference output:\n%s\nReason: %s\n",
            str, ref, infostr);
     fclose(fp);
     
     mock_input = freopen("mockinput", "w+", stdin);
     fputs("10200 a", mock_input);
     fseek(mock_input, 0, SEEK_SET);
     freopen("mockoutput", "w", stdout);
     freopen("erroroutput", "w", stderr);
     count_sum();
     fflush(stdout);
     fflush(stderr);
     fp = fopen("mockoutput", "r");
     FILE* fperror = fopen("erroroutput", "r");
     char strerror [100];
     memset(str, 0, sizeof(str));
     memset(strerror, 0, sizeof(strerror));
     fgets(str, 100, fp);
     fgets(strerror, 100, fperror);
     ref = "";
     char* referror = "error\n";
     infostr[0] = 0;
     char infoerror[100] = "";
     ret = mycompare(str, ref, infostr);
     fail_unless(!ret, "When giving input \"10200 a\", your output:\n%s\nReference output:\n%s\nReason: %s\n",
            str, ref, infostr);
     int reterror = mycompare(strerror, referror, infoerror);
     fail_unless(!reterror, "When giving input \"10200 a\", your error output:\n%s\nReference error output:\n%s\nReason: %s\n",
            strerror, referror, infoerror);
     fclose(fp);
}
END_TEST


int main(int argc, const char *argv[])
{
    srand((unsigned)time(NULL));
	Suite *s = suite_create("PrintErrors");

	tmc_register_test(s, test_count_sum, "printErrors");
        
	return tmc_run_tests(argc, argv, s);
}
