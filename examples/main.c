struct example {
    unsigned int x;
    unsigned int y;
};

int foo2(const int* x, volatile int y) {
    return 4;
}

int foo(int x, int y) {
    int z = x * 2 + y * 3;
    int a = x * 4;
    int b = y + 3;
    {
        int c = y + 3;
        int d = z + 2;
    }
    return z + a + b;
}

int foo3() {
    int z = 4;
    return z;
}

int main() {
    int y = 4;
    foo2(&y, 4);
    struct example s = { 5, 4 };
    unsigned char* ptr = (unsigned char*) 0xb8000;
    return 0;
}