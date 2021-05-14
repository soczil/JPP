#include <stdio.h>

int main(void) {
    int x = 5;

    if (x == 5) {
        printf("x = %d\n", x);
        x = 9;
        int x = 7;
        printf("x = %d\n", x);
        int y = 2;
        printf("y = %d\n", y);
    }
    printf("x = %d\n", x);
    // print("y = %d\n", y);

    return 0;
}