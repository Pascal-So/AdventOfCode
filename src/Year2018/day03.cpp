#include <bits/stdc++.h>

struct Rect {
    int top, left, height, width;
};

using Grid = std::vector<std::vector<int>>;

bool overlap(const Rect& r1, const Rect& r2) {
    if (r1.left > r2.left) {
        return overlap(r2, r1);
    }

    return r1.left + r1.width > r2.left &&
           r1.top + r1.height > r2.top &&
           r2.top + r2.height > r1.top;
}

int main() {
    std::vector<Rect> rects;

    int maxwidth = 0, maxheight = 0;

    char c;
    while (std::cin >> c) {
        Rect r;
        std::string s;
        std::cin >> s >> s
                 >> r.top
                 >> c
                 >> r.left
                 >> c
                 >> r.height
                 >> c
                 >> r.width;

        r.top++;
        r.left++;

        rects.push_back(r);

        maxwidth  = std::max(r.left + r.width , maxwidth);
        maxheight = std::max(r.top  + r.height, maxheight);
    }

    const int Width = maxwidth + 1;
    const int Height = maxheight + 1;

    Grid grid (Width, std::vector<int> (Height, 0));

    for (const auto& r : rects) {
        grid[r.left][r.top]++;
        grid[r.left][r.top + r.height]--;
        grid[r.left + r.width][r.top]--;
        grid[r.left + r.width][r.top + r.height]++;
    }

    int doubly_claimed = 0;
    for (int x = 1; x < Width; ++x) {
        for (int y = 1; y < Height; ++y) {
            grid[x][y] += grid[x][y-1] + grid[x-1][y] - grid[x-1][y-1];

            if (grid[x][y] > 1) {
                doubly_claimed ++;
            }
        }
    }

    std::cout << "Solution Part One: " << doubly_claimed << '\n';

    std::vector<bool> ok (rects.size(), true);

    for (std::size_t i = 0; i < rects.size(); ++i) {
        for (std::size_t j = i + 1; j < rects.size(); ++j) {
            if (!ok[i] && !ok[j]) continue;

            if (overlap(rects[i], rects[j])) {
                ok[i] = false;
                ok[j] = false;
            }
        }

        if (ok[i]) {
            std::cout << "Solution Part Two: " << i + 1 << '\n'; // < 1094
            break;
        }
    }
}