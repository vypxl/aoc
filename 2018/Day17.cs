using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

using Grid = System.Collections.Generic.List<System.Collections.Generic.List<Cell>>;
using Point = System.Tuple<int, int>;

class Cell {
    public bool blocked, visited;

    public override String ToString() {
        return (blocked ? (visited ? "\x1b[44m " : "\x1b[40m ") : (visited ? "\x1b[42m " : "\x1b[0m ")) + "\x1b[0m";
    }
}

class Day17 {
    static void Display(Grid grid) {
        for(int y = 0; y < grid[0].Count; y++) {
            for(int x = 0; x < grid.Count; x++) {
                Console.Write(grid[x][y]);
            }
            Console.WriteLine();
        }
    }

    // Fill one layer of a basin if it is blocked on both sides and has a floor
    static bool FillBasin(Grid grid, int initx, int y) {
        int x = initx;
        bool right = true;
        List<int> visited = new List<int>();
        // First go right, then left. If you find a hole or no blockade on one side, cancel
        while(true) {
            if (right) {
                x++;
                if (x >= grid.Count) return false;
                if (grid[x][y].blocked) {
                    right = false;
                }
            } else {
                x--;
                if (x < 0 || !grid[x][y + 1].blocked) return false;
                if (grid[x][y].blocked) break; 
                visited.Add(x);
            }
        }
        
        // Fill the layer
        visited.ForEach(_x =>  {
            grid[_x][y].blocked = true;
            grid[_x][y].visited = true;
        });
        return true;
    }

    // Simulate one falling square meter of water, mark the possible paths and fill the possible bottom layers of basins
    static bool Water(Grid grid, int sourcex) {
        int x = 0;
        int y = 0;

        // Mark all possible spots for the water to flow through
        Queue<Point> q = new Queue<Point>();
        q.Enqueue(new Point(sourcex, 1));
        HashSet<Point> seen = new HashSet<Point>();

        Point cur;
        bool res = false;
        while(q.TryDequeue(out cur)) {
            x = cur.Item1;
            y = cur.Item2;
            seen.Add(cur);

            // Out of grid
            if(grid[0].Count <= y || y < 0 || grid.Count <= x || x < 0 || grid[x][y].blocked) continue;
            res = res || !grid[x][y].visited;
            grid[x][y].visited = true;

            // Edge of grid
            if(grid[0].Count <= y + 1|| y < 0 || grid.Count <= x + 1 || x < 0 || grid[x][y].blocked) continue;

            // Cannot fall down
            if(grid[x][y + 1].blocked) {
                if(!seen.Contains(new Point(x + 1, y))) q.Enqueue(new Point(x + 1, y));
                if(!seen.Contains(new Point(x - 1, y))) q.Enqueue(new Point(x - 1, y));

                // Fill a possible basin
                res = FillBasin(grid, x, y) || res;
            } else if(!seen.Contains(new Point(x, y + 1))) q.Enqueue(new Point(x, y + 1));
        }
        return res;
    }

    static void Main(string[] args) {
        // Parsing
        List<string> f = File.ReadAllLines("17.in").ToList();
        int minx = 499;
        int maxx = 501;
        int miny = int.MaxValue;
        int maxy = 0;
        f.ForEach(l => {
            if (l != "") {
                var nums = Regex.Matches(l, @"\d+").Select(x => int.Parse(x.Value)).ToArray();
                if (l[0] == 'x') {
                    if (nums[0] < minx) minx = nums[0];
                    if (nums[0] > maxx) maxx = nums[0];
                    if (nums[1] < miny) miny = nums[1];
                    if (nums[1] > maxy) maxy = nums[1];
                    if (nums[2] < miny) miny = nums[2];
                    if (nums[2] > maxy) maxy = nums[2];
                } else {
                    if (nums[0] < miny) miny = nums[0];
                    if (nums[0] > maxy) maxy = nums[0];
                    if (nums[1] < minx) minx = nums[1];
                    if (nums[1] > maxx) maxx = nums[1];
                    if (nums[2] < minx) minx = nums[2];
                    if (nums[2] > maxx) maxx = nums[2];
                }
            }
        });
        miny--;
        minx--;
        maxx++;
        maxy++;
        int lenx = maxx - minx;
        int leny = maxy - miny;
        Grid grid = new Grid();
        for(int x = 0; x < lenx; x++) grid.Add(Enumerable.Range(0, leny).Select(_ => new Cell()).ToList());
        f.ForEach(l => {
            if (l != "") {
                var nums = Regex.Matches(l, @"\d+").Select(x => int.Parse(x.Value)).ToArray();
                if (l[0] == 'x')
                    for (int y = nums[1]; y <= nums[2]; y++) grid[nums[0] - minx][y - miny].blocked = true;
                else
                    for (int x = nums[1]; x <= nums[2]; x++) grid[x - minx][nums[0] - miny].blocked = true;
            }
        });

        // Simulate until no changes are made
        while(Water(grid, 500 - minx));

        // Output Solution
        Display(grid);
        Console.WriteLine("Solution for part 1:");
        Console.WriteLine(
            grid.Select<List<Cell>, int>(
                l => l.Where(x => x.visited).Count()
            ).Sum().ToString()
        );
        Console.WriteLine("Solution for part 2:");
        Console.WriteLine(
            grid.Select<List<Cell>, int>(
                l => l.Where(x => x.visited && x.blocked).Count()
            ).Sum().ToString()
        );
    }
}

// Solution for part 1: 30384
// Solution for part 2: 24479
