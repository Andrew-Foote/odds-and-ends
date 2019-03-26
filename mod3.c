#include <stdlib.h>
#include <time.h>

unsigned mod3(unsigned x) {
    static unsigned TABLE[48] = {
        0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2,
        0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2,
        0, 1, 2, 0, 1, 2
    };

    /* Sum adjacent 2-bit segments in parallel. Note that 0x33333333 is
    8 repetitions of the bit sequence 0011, and 0xcccccccc is 8
    repetitions of the bit sequence 1100. */
    x = (x & 0x33333333) + ((x & 0xcccccccc) >> 2);
    
    /* Sum adjacent 4-bit segments in parallel. Note that 0x0f0f0f0f is
    4 repetitions of the bit sequence 00001111, and 0xf0f0f0f0 is 4
    repetitions of the bit sequence 11110000. */
    x = (x & 0x0f0f0f0f) + ((x & 0xf0f0f0f0) >> 4);

    /* Sum adjacent 8-bit segments in parallel. Note that 0xff00ff00 is
    2 repetitions of the bit sequence 0000000011111111, and 0xf0f0f0f0
    is 2 repetitions of the bit sequence 1111111100000000. */
    x = (x & 0x00ff00ff) + ((x & 0xff00ff00) >> 8);

    /* Sum the two 16-bit segments. */
    x = (x & 0x0000ffff) + ((x & 0xffff0000) >> 16);

    return TABLE[x];
}

void profile_mod3() {
    static unsigned testcases[16777216];
    static unsigned testresults[16777216];

    for (int i = 0; i < 16777216; ++i)
        testcases[i] = i * 256 + rand() % 256;

    clock_t start = clock();

    for (int i = 0; i < 16777216; ++i) {
        testresults[i] = mod3(testcases[i]);
    }

    int diff = (clock() - start) * 1000 / CLOCKS_PER_SEC;
    printf("16777216 calls in %d ms\n", diff);

    for (int i = 0; i < 16777216; ++i) {
        if (testcases[i] % 3 != testresults[i]) {
            printf("Failed test case: %d\n", testcases[i]);
            exit(EXIT_FAILURE);
        }
    }
}

void profile_builtin_mod3() {
    static unsigned testcases[16777216];
    static unsigned testresults[16777216];

    for (int i = 0; i < 16777216; ++i)
        testcases[i] = i * 256 + rand() % 256;

    clock_t start = clock();

    for (int i = 0; i < 16777216; ++i) {
        testresults[i] = testcases[i] % 3;
    }

    int diff = ((clock() - start) * 1000) / CLOCKS_PER_SEC;
    printf("16777216 calls in %d ms\n", diff);
}

int main() {
    srand(time(NULL));
    printf("Profiling the old method...\n");
    profile_builtin_mod3();
    printf("Profiling the new method...\n");
    profile_mod3();
    return 0;
}