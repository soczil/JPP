#include <stdio.h>

void elomelo() {
    printf("jol");
}

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

    const int y = 10;
    printf("%d\n", y);

    for (int i = 0; i < 5; i++) {
        printf("jol %d\n", i);
    }
    int elomelo = 5;
    return 0;
    printf("eloo");
}