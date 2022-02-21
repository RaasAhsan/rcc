struct example {
    unsigned int x;
    unsigned int y;
};

int foo3() {
    int z = 4;
    int* h = &z;
    int b = *(h + 3);
    return b;
}

int x;

int bar(const int y) {

}

int main() {
    bar(x);
    int y = 4;
    struct example s = { 5, 4 };
    unsigned char* ptr = (unsigned char*) 0xb8000;
    return 0;
}