#include <stdint.h>

struct point {
    const int x;
    int y;
} p;

struct foo {};


int main() {
    const struct p {
        struct z {
            int f, g;
        } x;
        struct z y;
    }* z, *y, w = { 1, 2, 4, 5,};

    struct color {
        union {
            struct {
                uint8_t r, g, b, a;
            };
            uint32_t rgba;
        };
    } c;

    int* p;
    void* vp = (void*) (struct f { int x, y }*) p;
    struct f fp = { 2, 3};

    int x = 3;
    int* h = &x;
    int addr = (int) h;
    return addr;
}

struct point2 {
    const int x;
    int y;
};

int foo() {
    struct point2 p;
}