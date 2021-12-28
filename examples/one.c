#include <stdio.h>

int add(int x, int y) {
    return x + y;
}

int foo(int x) {
if (x) {
    return 3;
} else {
    return 4;
}
}

int main() {
    int z = foo(0);
    int* p = &z;
    char c = '3';
    char* cp = &c;
    int a = cp;
    printf("%d\n", a);
    return z;
}