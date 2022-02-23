struct point {
    const int x;
    int y;
} p;


int main() {
    struct {
        int x;
        int y;
    } z = { 3, 4 };
    struct {
        int x;
        int y;
    } r = { 3, 4 };
    struct {
        int x;
        int y;
    } g = { 3, 4 };

    int x = 3;
    int* h = &x;
    int addr = (int) h;
    return addr;
}