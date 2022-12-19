#include <bits/stdc++.h>
#include <execution>

using namespace std;

struct blueprint {
  int id, oR_oC, cR_oC, obR_oC, obR_cC, gR_oC, gR_obC;
};

struct state {
  int minute, ore, clay, obs, geo, oR, cR, obR, gR;
  auto operator<=>(const state &) const = default;
};

using memo_t = unordered_map<state, int>;

template <> struct std::hash<state> {
  size_t operator()(const state &s) const {
    const std::string str = std::string(
        reinterpret_cast<const std::string::value_type *>(&s), sizeof(state));
    return std::hash<std::string>()(str);
  }
};

ostream &operator<<(ostream &s, const blueprint &bp) {
  return s << '(' << bp.id << ", " << bp.oR_oC << ", " << bp.cR_oC << ", "
           << bp.obR_oC << ", " << bp.obR_cC << ", " << bp.gR_oC << ", "
           << bp.gR_obC << ')';
}

ostream &operator<<(ostream &s, const state &st) {
  return s << '(' << st.minute << ", " << st.ore << ", " << st.clay << ", "
           << st.obs << ", " << st.geo << ", " << st.oR << ", " << st.cR << ", "
           << st.oR << ", " << st.gR << ')';
}

static inline int MAX(int a, int b) {
  if (a > b)
    return a;
  return b;
}

struct problem {
  const blueprint &bp;
  int min_score;
  memo_t memo;
  int minutes;
  int maxO;

  int rec(state s) {
    if (s.minute == minutes)
      return s.geo;
    if (auto x = memo.find(s); x != memo.end()) {
      return x->second;
    }

    int min_left = minutes - s.minute;
    int score = s.geo + (s.gR * min_left);
    // cout << score << endl;
    if (score + (s.gR + min_left / 3) * min_left < min_score)
      return 0;
    if (s.oR > maxO || s.cR > bp.obR_cC || s.obR > bp.gR_obC)
      return 0;

    int x = 0;
    x = MAX(x, rec({s.minute + 1, s.ore + s.oR, s.clay + s.cR, s.obs + s.obR,
                    s.geo + s.gR, s.oR, s.cR, s.obR, s.gR}));
    if (s.ore >= bp.oR_oC)
      x = MAX(x,
              rec({s.minute + 1, s.ore - bp.oR_oC + s.oR, s.clay + s.cR,
                   s.obs + s.obR, s.geo + s.gR, s.oR + 1, s.cR, s.obR, s.gR}));
    if (s.ore >= bp.cR_oC)
      x = MAX(x,
              rec({s.minute + 1, s.ore - bp.cR_oC + s.oR, s.clay + s.cR,
                   s.obs + s.obR, s.geo + s.gR, s.oR, s.cR + 1, s.obR, s.gR}));
    if (s.ore >= bp.obR_oC && s.clay >= bp.obR_cC)
      x = MAX(x, rec({s.minute + 1, s.ore - bp.obR_oC + s.oR,
                      s.clay - bp.obR_cC + s.cR, s.obs + s.obR, s.geo + s.gR,
                      s.oR, s.cR, s.obR + 1, s.gR}));
    if (s.ore >= bp.gR_oC && s.obs >= bp.gR_obC)
      x = MAX(x, rec({s.minute + 1, s.ore - bp.gR_oC + s.oR, s.clay + s.cR,
                      s.obs - bp.gR_obC + s.obR, s.geo + s.gR, s.oR, s.cR,
                      s.obR, s.gR + 1}));

    memo[s] = x;
    min_score = MAX(min_score, x);
    return x;
  }

  int solve() { return rec({0, 0, 0, 0, 0, 1, 0, 0, 0}); }

  problem(const blueprint &bp, int minutes)
      : bp(bp), minutes(minutes), min_score(0), memo(),
        maxO(MAX(MAX(MAX(bp.oR_oC, bp.cR_oC), bp.obR_oC), bp.gR_oC)) {}
};

int part1(const vector<blueprint> &bps) {
  cout << "Starting sim for part 1..." << endl;
  vector<int> xs(bps.size());
  transform(std::execution::par_unseq, bps.cbegin(), bps.cend(), xs.begin(),
            [](const auto &bp) { return problem(bp, 24).solve(); });

  int mx = 0;
  for (int i = 0; i < xs.size(); ++i)
    mx += (i + 1) * xs[i];
  return mx;
}

int part2(const vector<blueprint> &bps) {
  cout << "Starting sim for part 2..." << endl;
  vector<int> xs(3);
  transform(std::execution::par_unseq, bps.cbegin(), bps.cbegin() + 3,
            xs.begin(), [](const auto &bp) { return problem(bp, 32).solve(); });

  return std::accumulate(xs.begin(), xs.end(), 1, std::multiplies<int>());
}

int main() {
  vector<blueprint> bps_ex = {{1, 4, 2, 3, 14, 2, 7}, {2, 2, 3, 3, 8, 3, 12}};

  FILE *fil = fopen("19.in", "r");
  vector<blueprint> bps;
  int a, b, c, d, e, f, g;
  while (fscanf(fil,
                "Blueprint %d: Each ore robot costs %d ore. Each clay robot "
                "costs %d ore. Each obsidian robot costs %d ore and %d clay. "
                "Each geode robot costs %d ore and %d obsidian.\n",
                &a, &b, &c, &d, &e, &f, &g) == 7)
    bps.push_back(blueprint{a, b, c, d, e, f, g});

  int p1 = part1(bps);
  int p2 = part2(bps);

  cout << "Solution for part 1: " << p1 << endl;
  cout << "Solution for part 2: " << p2 << endl;
}

// Solution for part 1: 1624
// Solution for part 2: 12628
