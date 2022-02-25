#include <stdint.h>

struct point {
    long x;
    float y;
} p;

struct foo {};

int pog(int x, int y) {

}

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
    
    pog(2, 3);

    struct point* p;

    return p->y;
}

struct point2 {
    const int x;
    int y;
};

int foo() {
    struct point2 p;
}