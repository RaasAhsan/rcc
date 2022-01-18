struct example {
    unsigned int x;
    unsigned int y;
};

int foo3() {
    int z = 4;
    int* h = &z;
    int b = *h;
    return b;
}

int main() {
    int y = 4;
    struct example s = { 5, 4 };
    unsigned char* ptr = (unsigned char*) 0xb8000;
    return 0;
}