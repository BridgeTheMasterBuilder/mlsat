#include <cmath>
#include <cstdint>

extern "C" int64_t value(int x, int64_t* m) {
    x >>= 1;
    int64_t y = m[std::abs(x)];
    y >>= 1;

    return (x < 0) ? ((-y) << 1) + 1 : (y << 1) + 1;
}
