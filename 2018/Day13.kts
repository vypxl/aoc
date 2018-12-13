#! /usr/bin/env kotlin
import java.nio.file.Files
import java.io.File

data class Cart(var x: Int, var y: Int, var direction: Int, var nextDir: Int)

typealias Grid = Array<IntArray>
typealias Carts = MutableList<Cart>

val UP: Int    = 0
val RIGHT: Int = 1
val DOWN: Int  = 2
val LEFT: Int  = 3

val NONE: Int         = 4
val VERTICAL: Int     = 5
val HORIZONTAL: Int   = 6
val CURVERIGHT: Int   = 7
val CURVELEFT: Int    = 8
val INTERSECTION: Int = 9

fun parse(lines: List<String>): Pair<Grid, Carts> {
    val grid = Array(lines[0].length, { IntArray(lines.size) })
    var carts = mutableListOf<Cart>()
    for (y in lines.indices) {
        for (x in lines[y].indices) {
            grid[x][y] = when (lines[y][x]) {
                in " " -> NONE

                in "|" -> VERTICAL
                in "^" -> { carts.add(Cart(direction = UP,    x = x, y = y, nextDir = LEFT)); VERTICAL }
                in "v" -> { carts.add(Cart(direction = DOWN,  x = x, y = y, nextDir = LEFT)); VERTICAL }

                in "-" -> HORIZONTAL
                in "<" -> { carts.add(Cart(direction = LEFT,  x = x, y = y, nextDir = LEFT)); HORIZONTAL }
                in ">" -> { carts.add(Cart(direction = RIGHT, x = x, y = y, nextDir = LEFT)); HORIZONTAL }

                in "\\" -> CURVELEFT
                in "/" -> CURVERIGHT
                in "+" -> INTERSECTION
                else -> NONE
            }
        }
    }
    return Pair(grid, carts)
}

fun Cart.move() {
    when(this.direction) {
        UP ->    this.y -= 1
        RIGHT -> this.x += 1
        DOWN ->  this.y += 1
        LEFT ->  this.x -= 1
        else -> throw Exception("")
    }
}

var foundSolPart1 = false

fun tick(grid: Grid, carts: Carts) {
    var toRemove = mutableListOf<Cart>()
    for (cart in carts.sortedWith(compareBy({ it.y }, { it.x }))) {
        cart.move()
        
        when (grid[cart.x][cart.y]) {
            INTERSECTION -> {
                cart.direction = (cart.direction + cart.nextDir) % 4
                cart.nextDir = when (cart.nextDir) {
                        LEFT -> UP
                        UP -> RIGHT
                        RIGHT -> LEFT
                        else -> throw Exception("")
                    }
            }
            VERTICAL, HORIZONTAL-> { }
            CURVERIGHT -> cart.direction = when (cart.direction) {
                    UP -> RIGHT
                    RIGHT -> UP
                    DOWN -> LEFT
                    LEFT -> DOWN
                    else -> throw Exception("")
            }
            CURVELEFT -> cart.direction = when (cart.direction) {
                    UP -> LEFT
                    LEFT -> UP
                    DOWN -> RIGHT
                    RIGHT -> DOWN
                    else -> throw Exception("")
            }
            else -> throw Exception("")
        }
        
        for (c in carts)
            if (c != cart && c.x == cart.x && c.y == cart.y) {
                if(!foundSolPart1) {
                    println("Solution for part 1:")
                    println("${c.x},${c.y}")
                    foundSolPart1 = true
                }
                toRemove.add(c)
                toRemove.add(cart)
            }
    }
    for (r in toRemove)
        carts.remove(r)
}

val f = Files.readAllLines(File("13.in").toPath())
var (grid, carts) = parse(f)

while(true) {
    tick(grid, carts)
    if (carts.size == 1) {
        println("Solution for part 2:")
        println("${carts[0].x},${carts[0].y}")
        break
    }
}

// Solution part 1: 117,62
// Solution part 2: 69,67
