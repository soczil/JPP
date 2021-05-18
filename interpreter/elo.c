#include <stdio.h>

int elomelo() {
    int x = 5;
    printf("jol");
    int elomelo = 2;
    return 0;
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


    for (int i = 0; i < 5; i++) {
        printf("jol %d\n", i);
    }
    const int elomelo = 1;

    int j;
    for (j = 0; j < 5; j++) {
        if (j == 4) {
            break;
        }
    }
    printf("aaaaaa %d\n", j);


    int y = 2;    
    while (y <= 5) {
        if (y == 3) {
            continue;
        }
        printf("%d\n", y);
        y++;
    }



    return 0;
    for (int i = 0; i < 10; i++) {
        printf("jol");
    }
    printf("eloo");
}