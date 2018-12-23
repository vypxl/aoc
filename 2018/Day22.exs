#! /usr/bin/env elixir
defmodule Day22 do
    # Calculates the erosion level of a region by it's geologic index and the depth
    def elevel(d, x), do: rem(d + x, 20183)

    # Generates a column of the cave with the depth, it's x coordinate and the previous column
    def gencol(d, x, prevcol), do:
        # The first region is irrelevant
        tl(prevcol) 
        # Generate the column via reduce and start with the region at y = 0
        |> Enum.reduce([elevel(d, x * 16807)], 
            fn y, [prev | col] -> [elevel(d, y * prev) | [prev | col]] end)
        # Reverse the column because everything got prepended
        |> Enum.reverse

    # Generates the cave map based on it's depth, the maximum x and y coordinates and the target coordinates
    def genmap(d, maxx, maxy, tx, ty), do:
        # Generate columns one after another
        1..maxx
        # By reduce, starting with the column at x = 0 which does not depend on another column
        |> Enum.reduce([0..maxy |> Enum.map(fn y -> elevel(d, y * 48271) end)],
            # Call gencol with the depth, x coordinate and the previous column
            fn x, [prevcol | cols] -> [gencol(d, x, prevcol) | [prevcol | cols]] end)
        # Reverse the columns because everything got prepended
        |> Enum.reverse
        # Set the target to erosion level correctly
        |> List.update_at(tx, &List.replace_at(&1, ty, elevel(d, 0)))
        # Convert erosion levels to region types
        |> Enum.map(fn col -> Enum.map(col, &rem(&1, 3)) end)

    def transpose(rows), do:
        rows |> Enum.zip |> Enum.map(&Tuple.to_list/1)

    def display(grid, tx, ty), do:
        IO.ANSI.format([
            :black_background,
            Enum.map(transpose(
                grid
                |> List.update_at(0, &List.replace_at(&1, 0, 4))
                |> List.update_at(tx, &List.replace_at(&1, ty, 5))
                ), 
                &Enum.map(&1 ++ [3], fn item -> 
                    case item do
                        0 -> IO.ANSI.format([:black_background, :light_black, "#"], true)
                        1 -> IO.ANSI.format([:black_background, :cyan, "="], true)
                        2 -> IO.ANSI.format([:black_background, :red,  "|"], true)
                        3 -> "\n"
                        4 -> IO.ANSI.format([:black_background, :green,  :bright, "X"], true)
                        5 -> IO.ANSI.format([:black_background, :yellow, :bright, "T"], true)
                    end
                end)
            )
        ], true)

    def risklevel(d, tx, ty), do:
        genmap(d, tx, ty, tx, ty) |> Enum.map(&Enum.sum/1) |> Enum.sum

    def getgraph(grid) do
        w = length grid
        h = length (hd grid)
        coords = Enum.flat_map(0..w-1, fn x -> 
            Enum.flat_map(0..h-1, fn y -> 
                rt = grid |> Enum.at(x) |> Enum.at(y)
                case rt do
                    0 -> [{x, y, :torch}, {x, y, :climb}]
                    1 -> [{x, y, :climb}, {x, y, :neith}]
                    2 -> [{x, y, :torch}, {x, y, :neith}]
                end
            end)
        end)
        neighs = fn x, y -> 
            [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
            |> Enum.filter(fn {x, y} -> x >= 0 && y >= 0 && x < w && y < h end)
        end

        Map.new Enum.map(coords, fn {x, y, tool} -> 
            ctype = grid |> Enum.at(x) |> Enum.at(y)
            toolswitch = case {ctype, tool} do
                {0, :torch} -> {{x, y, :climb}, 7}
                {0, :climb} -> {{x, y, :torch}, 7}
                {1, :neith} -> {{x, y, :climb}, 7}
                {1, :climb} -> {{x, y, :neith}, 7}
                {2, :neith} -> {{x, y, :torch}, 7}
                {2, :torch} -> {{x, y, :neith}, 7}
            end
            {{x, y, tool}, [toolswitch | Enum.flat_map(neighs.(x, y), fn {nx, ny} ->
                ntype = grid |> Enum.at(nx) |> Enum.at(ny)
                case {ntype, ctype, tool} do
                    {^ctype, _, _} -> [{{nx, ny, tool}, 1}]
                    {0, 1, :climb} -> [{{nx, ny, :climb}, 1}]
                    {0, 2, :torch} -> [{{nx, ny, :torch}, 1}]
                    {1, 0, :climb} -> [{{nx, ny, :climb}, 1}]
                    {1, 2, :neith} -> [{{nx, ny, :neith}, 1}]
                    {2, 0, :torch} -> [{{nx, ny, :torch}, 1}]
                    {2, 1, :neith} -> [{{nx, ny, :neith}, 1}]
                    _ -> []
                end
            end)]}
        end)
    end

    def adjust(dist, u, {v, dv}) do
        alt = Map.get(dist, u) + dv
        if alt < Map.get(dist, v), do: Map.replace!(dist, v, alt), else: dist
    end

    # def dodijkstra(_, %MapSet{}, dist, prev, _), do: {dist, prev}
    def dodijkstra(graph, q, dist, target) do
        {u, _} = Enum.reduce(q, {nil, :infinity}, 
                fn node, {anode, adist} -> 
                    ndist = Map.get(dist, node)
                    if ndist < adist, do: {node, ndist}, else: {anode, adist}
                end)
        IO.inspect Map.get(dist, u)

        if u == target, do: dist, else: dodijkstra(
            graph,
            MapSet.delete(q, u),
            Enum.reduce(
                Enum.filter(Map.get(graph, u), &MapSet.member?(q, elem(&1, 0))), 
                dist, 
                &adjust(&2, u, &1)),
            target
        )
    end

    def dijkstra(grid, target) do
        graph = getgraph(grid)
        dodijkstra(
            graph,
            MapSet.new(Map.keys graph),
            Map.new(Enum.map Map.keys(graph), fn c -> {c, :infinity} end) |> Map.replace!({0, 0, :torch}, 0),
            target
        )
    end

    def time(d, tx, ty) do
        grid = genmap(d, tx + 100, ty + 100, tx, ty)
        dist = dijkstra(grid, {tx, ty, :torch})
        
        Map.get(dist, {tx, ty, :torch})
    end

    def main do
        {_, f} = File.read "22.in"
        [depth, tx, ty] = Enum.map(Regex.scan(~r/\d+/, f), fn x -> {v, _} = Integer.parse(hd x); v end)
        # [depth, tx, ty] = [510, 10, 10]
        
        IO.puts "Solution for part 1:"
        IO.puts risklevel(depth, tx, ty)
        IO.puts "Solution for part 2:"
        IO.puts time(depth, tx, ty)
    end
end

Day22.main

# This run VERY slowly. Took about an hour for part 2. I don'thave time for optimizing :(
# Solution for part 1: 10395
# Solution for part 2: 1010
