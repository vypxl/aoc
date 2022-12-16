#include <bits/stdc++.h>
using namespace std;

using num_t = unsigned long;
using memo_key_t = tuple<num_t, num_t>;

struct hash_tuple {
    template <class T1, class T2>
    size_t operator()(const tuple<T1, T2>& x) const {
      return get<0>(x) ^ get<1>(x);
    }
};

using Memo = unordered_map<memo_key_t, short, hash_tuple>;

constexpr num_t N = 26 * 52 * 52 * (1l << 15);

num_t walk(num_t n, Memo& memo, vector<num_t>& flows, vector<vector<num_t>>& neighbours, num_t k, num_t p1, num_t p2, num_t o) {
  if (k == 0) return 0;
  memo_key_t key(k << 20l | p1 << 10l | p2, o);
  if (auto x = memo.find(key); x != memo.end()) return x->second;

  num_t res = 0;

  if (k > 1) {
    vector<tuple<num_t, num_t>> choices1, choices2;

    // if current valve can be opened
    if ((not (o & (1l << p1))) && flows[p1] > 0)
        choices1.emplace_back(make_tuple(p1, o | (1l << p1)));
    for (auto& n : neighbours[p1])
        choices1.emplace_back(make_tuple(n, o));
    if ((not (o & (1l << p2))) && flows[p2] > 0)
        choices2.emplace_back(make_tuple(p2, o | (1l << p2)));
    for (auto& n : neighbours[p2])
        choices2.emplace_back(make_tuple(n, o));

    for (auto& [np1, no1] : choices1) {
        for (auto& [np2, no2] : choices2) {
          num_t x = walk(n, memo, flows, neighbours, k-1, np1, np2, no1 | no2);
          if (x > res) res = x;
      }
    }
  }

  for (num_t i = 0; i < n; ++i) {
      res += flows[i] * !!(o & (1l << i));
  }
  memo[key] = res;

  if (memo.size() % 1000000 == 0)
      cout << (memo.size() / (float) N * 100) << "% | " << memo.size() << " / " << N << endl;

  return res;
}

num_t solve(num_t n, vector<num_t> flows, vector<vector<num_t>> neighbours, num_t start) {
  Memo memo;

  // walk(graph, k, position, opened_valves) returns the maximum flow achievable in k minutes
  return walk(n, memo, flows, neighbours, 26, start, start, 0);
}

int main() {
  num_t n_ex = 10;
  vector<num_t> flows_ex = { 0, 13, 2, 20, 3, 0, 0, 22, 0, 21 };
  vector<vector<num_t>> neighbours_ex = {{3, 8, 1}, {2, 0}, {3, 1}, {2, 0, 4}, {5, 3}, {4, 6}, {5, 7}, {6}, {0, 9}, {8}};
  num_t start_ex = 0;

  num_t n = 52;
  vector<num_t> flows = { 0, 21, 0, 0, 16, 0, 0, 0, 10, 0, 17, 0, 0, 24, 0, 0, 0, 0, 20, 0, 0, 13, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 25, 0, 0, 0, 0, 0, 18, 0, 0, 9, 0, 23, 12, 3 };
  vector<vector<num_t>> neighbours = {{14, 36}, {46, 39}, {51, 14}, {34, 50}, {29}, {15, 35}, {25, 50}, {27, 44}, {20, 45, 19}, {39, 21}, {28, 25}, {41, 47}, {31, 51}, {42, 26}, {2, 0, 37, 17, 40}, {5, 47}, {51, 29}, {14, 48}, {46}, {27, 8}, {21, 8}, {20, 23, 9, 36}, {42, 44}, {21, 27}, {51, 43}, {6, 10}, {13, 49}, {19, 23, 40, 7}, {33, 10}, {16, 4}, {47, 38}, {12, 35}, {51, 50}, {28, 49}, {35, 3}, {34, 5, 31, 37}, {0, 21}, {14, 35}, {30}, {9, 1}, {27, 14}, {50, 11}, {22, 13}, {24, 47}, {22, 45, 7}, {44, 8}, {1, 18}, {48, 30, 43, 11, 15}, {17, 47}, {33, 26}, {3, 41, 6, 32}, {2, 16, 24, 12, 32}};
  num_t start = 14;

  printf("Solution for part 2: %lu\n", solve(n_ex, flows_ex, neighbours_ex, start_ex));
  printf("Solution for part 2: %lu\n", solve(n, flows, neighbours, start));
  return 0;
}

// Runs in 810 secs lol

// Solution part 1: 1862
// Solution part 2: 2422
