
int add(int a, int b) {
    return a + b;
}

int main() {
    int (*fptr)(int, int) = add;
    // char* x, y, **z;
    // printf("%d, %d, %d", sizeof(x), sizeof(y), sizeof(z));
    return 0;
}