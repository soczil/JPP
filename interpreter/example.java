// przyklad 1
// HelloWorld.lpp
int main() {
    string hello = "HelloWorld!";
    print(hello);
    return 0;
}

// przyklad 2
// PrintArrayElements.lpp
int main() {
    Array<int> xs = 5 ** [1]; // utworzenie tablicy postaci [1, 1, 1, 1, 1]
    for (x : xs) {
        print(x);
    }

    for (int i = 0; i < 5; i++) {
        print(xs[i]);
    }

    int i = 0;
    while (i < 5) {
        print(xs[i]);
    }
    return 0;
}

// przyklad 3
// PrintEvenNumbers.lpp
int main() {
    bool isEven(int x) {
        if (x % 2 == 0) {
            return true;
        } else {
            return false;
        } 
    }

    Array<int> numbers = 10 ** [0];
    for (int i = 1; i <= 10; i++) {
        numbers[i - 1] = i;
    }

    for (x : numbers) {
        if (isEven(x)) {
            print(x);
        }
    }
    return 0;
}

// przyklad 4
// Fib.lpp (obliczanie n-tej liczby Fibonacciego na trzy sposoby)
int fib_rec(int n) {
    if (n <= 1) {
        return n;
    }
    return fib_rec(n - 1) + fib_rec(n - 2);
}

int main() {
    int n = 10;
    print(fib_rec(n));
    print(fib_arr(n));
    print(fib_opt(n));
    return 0;
}

int fib_arr(int n) {
    Array<int> f = (n + 2) ** [0];
    int i = 2;

    f[1] = 1;
    while (i <= n) {
        f[i] = f[i - 1] + f[i - 2];
        i++;
    }

    return f[n];
}

int fib_opt(int n) {
    int f1 = 0, f2 = 1, res;
    if (n == 0) {
        return f1;
    }

    for (int i = 2; i <= n; i++) {
        res = f1 + f2;
        f1 = f2;
        f2 = res;
    }

    return f2;
}