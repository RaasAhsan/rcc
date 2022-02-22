
int main() {
    int a = 3;
    int* ptr = &a;
    long addr = (long) ptr;
    return addr;
}