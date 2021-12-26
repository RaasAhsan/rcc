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
return z;
}