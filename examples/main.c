
int main() {
    long l = 0x2000;
    unsigned int* ptr = (unsigned int*) l;
    return *ptr;
}