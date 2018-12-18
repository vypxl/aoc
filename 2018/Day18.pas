#! /usr/bin/env instantfpc
program Day18;

uses
    Sysutils;

const
    ground = 0;
    trees = 1;
    lumber = 2;
    w = 49;
    h = 49;
    repr: array[0..2] of char = ('.', '|', '#');

var
    f: TextFile;
    curc: char;
    grid: array[0..w, 0..h] of integer;
    x: integer = 0;
    y: integer = 0;
    i, j, k: integer;
    nt, nl, ps: integer;

procedure display();
var
    i, j: integer;
begin
    for j := 0 to w do begin
        for i := 0 to h do begin
            write(repr[grid[i, j]])
        end;
        writeln();
    end;
end;

procedure minute();
var
    x, y, dx, dy: integer;
    nground, ntrees, nlumber: integer;
    ngrid: array[0..w, 0..h] of integer;
begin
    for x := 0 to w do begin
        for y := 0 to h do begin
            nground := 0;
            ntrees  := 0;
            nlumber := 0;
            for dx := -1 to 1 do
                for dy := -1 to 1 do begin
                    if ((not ((dx = 0) and (dy = 0))) and (x + dx >= 0) and (x + dx <= w) and (y + dy >= 0) and (y + dy <= h)) then
                        case grid[x + dx, y + dy] of
                            ground : nground := nground + 1;
                            trees : ntrees := ntrees + 1;
                            lumber : nlumber := nlumber + 1;
                        end;
                end;
            if grid[x, y] = ground then begin
                if ntrees >= 3 then ngrid[x, y] := trees
                else ngrid[x, y] := ground;
            end;
            if grid[x, y] = trees then begin
                if nlumber >= 3 then ngrid[x, y] := lumber
                else ngrid[x, y] := trees;
            end;
            if grid[x, y] = lumber then begin
                if ((nlumber >= 1) and (ntrees >= 1)) then ngrid[x, y] := lumber
                else ngrid[x, y] := ground;
            end;
        end;
    end;
    grid := ngrid;
end;

begin
    assign(f, '18.in');
    reset(f);
    while not eof(f) do
    begin
        read(f, curc);
        if curc = '.' then
            grid[x, y] := ground;
        if curc = '|' then
            grid[x, y] := trees;
        if curc = '#' then
            grid[x, y] := lumber;
        if curc = #10 then begin
            y := y + 1;
            x := -1
        end;
        x := x + 1;
    end;
    close(f);

    ps := 0;
    for k := 1 to 489 + 29 do begin
        write(k, ': ');
        // display;
        minute;
        nt := 0;
        nl := 0;
        for j := 0 to w do
            for i := 0 to h do begin
                if (grid[i, j] = trees) then nt := nt + 1;
                if (grid[i, j] = lumber) then nl := nl + 1
            end;
        writeln(nt * nl);

    end;
end.

// Part 2: Reoccurring pattern starting at minute 489 with length 29, starting with 192100
// After minute 0..488 => Just take the m'th value
// After minute 489+ => take the (((m - 489) % 28) + 489)'th value
// So for m = 1000000000 -> ((m - 489) % 28) + 489 = 496

// Solution part 1: 646437
// Solution part 2: 2080080
