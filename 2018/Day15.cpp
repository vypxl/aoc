#include <iostream>
#include <ostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <map>
#include <numeric>

using namespace std;
using grid_t = vector<vector<int>>;

enum unit_t { WALL, FREE, ELF, GOBLIN };
const string unit_s = "#.EG";

struct point {
public:
    int x;
    int y;

    int mandist(point other);
};

const point neighbours[4] = { point{-1, 0}, point{1, 0}, point{0, -1}, point{0, 1} };

point operator+(const point& a, const point& b) {
    return point{ a.x + b.x, a.y + b.y };
}

bool operator==(const point& a, const point& b) {
    return a.x == b.x && a.y == b.y;
}

bool operator<(const point& a, const point& b) {
    if(a.y == b.y) return a.x < b.x;
    return a.y < b.y;
}

int point::mandist(point other) {
    return abs(this->x - other.x) + abs(this->y - other.y);
}

ostream& operator<<(ostream& os, const point& p) {
    return os << "(" << p.x << "|" << p.y << ")";
}

struct unit {
public:
    unit_t kind;
    point pos;
    int atk;
    int hp;

    bool attack(vector<unit> &units);
    bool isenemy(const unit& other);
    void move(const grid_t& grid, const vector<unit> units);
};

ostream & operator<<(ostream& os, const unit& u) {
    return os << "Unit { " << unit_s[(int) u.kind] << " at " << u.pos << ", atk: " << u.atk << ", hp: " << u.hp << " }";
}

bool operator==(const unit& a, const unit& b) {
    return a.kind == b.kind && a.atk == b.atk && a.pos == b.pos && a.hp == b.hp;
}

bool unit::isenemy(const unit& other) {
    return this->kind != other.kind;
}

bool unit::attack(vector<unit> &units) {
    // Abort if all enemies are dead
    if (none_of(units.begin(), units.end(), [this](unit u) { return u.isenemy(*this) && u.hp > 0; })) return false;

    vector<unit*> adjacent;
    // List adjacent units
    for (auto& u : units) if (u.isenemy(*this) && u.hp > 0 && u.pos.mandist(this->pos) == 1) adjacent.push_back(&u);
    // Abort if there are no units in range
    if (adjacent.size() == 0) return true;
    // HP, then reading order
    sort(adjacent.begin(), adjacent.end(), [this](unit *a, unit *b) {
       if (a->hp == b->hp)
           return a->pos < b->pos;
       return a->hp < b->hp;
    });
    
    adjacent[0]->hp -= this->atk;
    return true;
}

bool blocked(const vector<unit> &units, const point& p) {
    for (auto& u : units) if (u.pos == p && u.hp > 0) return true;
    return false;
}

// Using a variation of dijkstra's algorithm, compute movement
void unit::move(const grid_t& grid, const vector<unit> units) {
    vector<point> q;
    map<point, int> dist;
    // Set up dist map
    for (int y = 0; y < grid[0].size(); y++) for (int x = 0; x < grid.size(); x++) dist[point { x, y }] = -1;
    q.push_back(this->pos);
    dist[this->pos] = 0;

    // Assign each point it's distance
    while(!q.empty()) {
        point cur = q.back();
        q.pop_back();

        for (point n : neighbours) {
            point p = cur + n;
            if (grid[p.x][p.y] == FREE && !blocked(units, p) && (dist[p] == -1 || dist[p] > dist[cur] + 1)) {
                q.push_back(p);
                dist[p] = dist[cur] + 1;
            }
        }
    }
    
    // Gather nearest targets
    vector<point> targets;
    for (auto& u : units) if (this->isenemy(u) && u.hp > 0) for (auto& p : neighbours) { 
        point cur = u.pos + p;
        if(grid[cur.x][cur.y] == FREE && !blocked(units, cur)) targets.push_back(cur); 
    }
    int nearest_dist = 0xffff;
    for (point t : targets) if (dist[t] != -1 && dist[t] < nearest_dist) nearest_dist = dist[t];

    vector<point> nearest;
    for (point t : targets) if (dist[t] == nearest_dist) nearest.push_back(t);

    // If there is no path, do not move
    if(nearest.empty()) return;

    // Choose target
    sort(nearest.begin(), nearest.end());
    point chosen = nearest[0];



    // Compute distances again, but now with the chosen point as the start
    for (int y = 0; y < grid[0].size(); y++) for (int x = 0; x < grid.size(); x++) dist[point { x, y }] = -1;
    q.push_back(chosen);
    dist[chosen] = 0;

    // Again, assign each point it's distance
    while(!q.empty()) {
        point cur = q.back();
        q.pop_back();

        for (point n : neighbours) {
            point p = cur + n;
            if (grid[p.x][p.y] == FREE && !blocked(units, p) && (dist[p] == -1 || dist[p] > dist[cur] + 1)) {
                q.push_back(p);
                dist[p] = dist[cur] + 1;
            }
        }
    }

    // Get neighbour distances
    vector<int> neighbour_dists;
    for (point n : neighbours) neighbour_dists.push_back(dist[this->pos + n]);

    // Get minimum distance
    int min_dist = 0xffffff;
    for (int i : neighbour_dists) if (i != -1 && i < min_dist) min_dist = i;

    // Collect possible neighbours
    vector<point> possible;
    for (point p : neighbours) if (dist[this->pos + p] == min_dist) possible.push_back(this->pos + p);

    // Finally determine where to move
    sort(possible.begin(), possible.end());

    this->pos = possible[0];
}

string display(grid_t grid, vector<unit> units) {
    string s = "";
    
    for (int y = 0; y < grid[0].size(); y++) {
        for (int x = 0; x < grid.size(); x++)
            s += unit_s[grid[x][y]];
        s += "\n";
    }

    for (auto u : units)
        if(u.hp > 0)
            s[u.pos.x + u.pos.y * (grid.size() + 1)] = unit_s[u.kind];
    
    return s;
}

bool round(grid_t &grid, vector<unit> &units) {
    sort(units.begin(), units.end(), [](unit a, unit b) { return a.pos < b.pos; });
    for (auto& u : units) {
        // Am I dead?
        if (u.hp <= 0) continue;

        // Is there an enemy in range?
        if (!any_of(units.begin(), units.end(), [&u](unit _u) { return u.isenemy(_u) && _u.hp > 0 && u.pos.mandist(_u.pos) == 1; }))
            u.move(grid, units);
        if(!u.attack(units)) return false;
    }
    return true;
}

int main(int argc, char** argv) {
    int ELF_ATK = 3;
    if(argc == 2) ELF_ATK = atoi(argv[1]);
    ifstream fs("15.in");
    string f;
    getline(fs, f, (char) fs.eof());

    // Parse input
    int height = count(f.begin(), f.end(), '\n');
    int width  = (int) f.find("\n");
    grid_t grid = {};
    for (int x = 0; x < width; x++)
        grid.push_back(vector<int>());

    for (int x = 0; x < width; x++) 
        for(int y = 0; y < height; y++) 
            grid[x].push_back((int) unit_s.find(f[x + y * (width + 1)]));
    
    int elfcount = 0;
    vector<unit> units = {};
    for (int x = 0; x < width; x++)
        for(int y = 0; y < height; y++)
            if(grid[x][y] == ELF || grid[x][y] == GOBLIN) {
                int atk = 3;
                if(grid[x][y] == ELF) {
                    atk = ELF_ATK;
                    elfcount++;
                }
                units.push_back(unit{ (unit_t) grid[x][y], point {x, y}, atk, 200 });
                grid[x][y] = FREE;
            }

    // Run until one team is dead
    int i = 0;
    while (1) {
        i++;
        cout << "Round " << i << endl << display(grid, units) << endl;
        bool complete = round(grid, units);

        // count units
        int elfs = 0;
        int goblins = 0;
        for (auto& u : units) if (u.hp > 0) { if(u.kind == ELF) elfs++; else goblins++; }
        if (elfs == 0 || goblins == 0) {
            int all_hp = 0;
            int rounds = i;
            if(!complete) rounds--;
            for (auto& u : units) if (u.hp > 0) all_hp += u.hp;
            cout << "Solution: " << all_hp * rounds << endl << "All hp: " << all_hp << endl << "Rounds: " << rounds << endl << "Part 2 satisfied: " << (elfcount == elfs) << endl;
            break;
        }
    }
}

// For part 1, run the executable without arguments
// For part 2, run the executable with the desired Elf attack value and adjust it until you find the answer
// My tries were 100 -> 50 -> 25 -> 15 -> 20 -> 17 -> 18 -> 19
// => I used Elf attack power 19 for part 2

// Solution part 1: 229798
// Solution part 2: 52972
