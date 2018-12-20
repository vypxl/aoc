#! /usr/bin/env -S dmd -run

import std.algorithm, std.stdio, std.file, std.range, std.conv;

const int N     = 0b00001;
const int E     = 0b00010;
const int S     = 0b00100;
const int W     = 0b01000;
const int START = 0b10000;

struct Point {
    int x;
    int y;
    int directions;
}

alias Grid = int[int][int];

Grid parse(string s) {
    Grid grid;
    Point[] stack = [Point(0, 0)];
    Point cur = Point(0, 0);

    for (ulong i = 0; i < s.length; i++) {
        switch (s[i]) {
            case 'N':
                grid[cur.x][cur.y] |= N;
                cur = Point(cur.x, cur.y + 1, S);
                break;
            case 'S':
                grid[cur.x][cur.y] |= S;
                cur = Point(cur.x, cur.y - 1, N);
                break;
            case 'E':
                grid[cur.x][cur.y] |= E;
                cur = Point(cur.x + 1, cur.y, W);
                break;
            case 'W':
                grid[cur.x][cur.y] |= W;
                cur = Point(cur.x - 1, cur.y, E);
                break;
            case '(':
                stack ~= cur;
                break;
            case ')':
                cur = stack.back;
                stack.popBack();
                break;
            case '|':
                cur = stack.back;
                break;
            default: break;
        }
        grid[cur.x][cur.y] |= cur.directions;
    }
    
    grid[0][0] |= START;
    return grid;
}

string display(Grid grid) {
    string s = "";
    int maxx, minx, maxy, miny = 0;
    foreach (x; grid.keys)
        foreach (y; grid[x].keys) {
            if (x > maxx) maxx = x;
            if (x < minx) minx = x;
            if (y > maxy) maxy = y;
            if (y < miny) miny = y;
        }
    for (int y = maxy; y >= miny ; y--) {
        for (int x = minx; x <= maxx; x++) {
            wchar c = '#';
            int g = 0;
            if(x in grid && y in grid[x]) g = grid[x][y];
            switch (g) {
                case 0:
                    c = '#';
                    break;
                case N:
                    c = '╵';
                    break;
                case E:
                    c = '╶';
                    break;
                case N | E:
                    c = '╰';
                    break;
                case S:
                    c = '╷';
                    break;
                case N | S:
                    c = '│';
                    break;
                case E | S:
                    c = '╭';
                    break;
                case N | E | S:
                    c = '├';
                    break;
                case W:
                    c = '╴';
                    break;
                case N | W:
                    c = '╯';
                    break;
                case E | W:
                    c = '─';
                    break;
                case N | E | W:
                    c = '┴';
                    break;
                case S | W:
                    c = '╮';
                    break;
                case N | S | W:
                    c = '┤';
                    break;
                case E | S | W:
                    c = '┬';
                    break;
                case N | E | S | W:
                    c = '┼';
                    break;
                default: 
                    c = '╳';
                    break;
            }
            s ~= c;
        }
        s ~= '\n';
    }

    return s;
}

Point[] neighbours(Point p) {
    Point[] ps;
    if (p.directions & N) ps ~= Point(p.x    , p.y + 1, p.directions);
    if (p.directions & E) ps ~= Point(p.x + 1, p.y    , p.directions);
    if (p.directions & S) ps ~= Point(p.x    , p.y - 1, p.directions);
    if (p.directions & W) ps ~= Point(p.x - 1, p.y    , p.directions);
    return ps;
}

Grid distances(Grid grid) {
    Grid dist;
    Point[] q;

    foreach (x; grid.keys) 
        foreach (y; grid[x].keys) {
            dist[x][y] = int.max;
            q ~= Point(x, y, grid[x][y]);
        }
    dist[0][0] = 0;

    while (!q.empty) {
        ulong idx = q.map!(p => dist[p.x][p.y])().minIndex;
        Point u = q[idx];
        q = q.remove(idx);
        foreach (v; neighbours(u)) {
            int alt = dist[u.x][u.y] + 1;
            if (alt < dist[v.x][v.y]) dist[v.x][v.y] = alt;
        }
    }

    return dist;
}

int part1(Grid dist) {
    int max = 0;
    foreach (x; dist.keys) 
        foreach (y; dist[x].keys) {
            if(dist[x][y] > max) max = dist[x][y];
        }
    return max;
}

int part2(Grid dist) {
    int count = 0;
    foreach (x; dist.keys) 
        foreach (y; dist[x].keys) {
            if(dist[x][y] >= 1000) count++;
        }
    return count;
}

void main() {
    string f = readText("20.in").drop(1).dropBack(2);
    Grid grid = parse(f);
    Grid dist = distances(grid);

    writeln(display(grid));
    writeln("Solution for part 1:");
    writeln(part1(dist));
    writeln("Solution for part 2:");
    writeln(part2(dist));
}

// Solution part 1: 3991
// Solution part 2: 8394
