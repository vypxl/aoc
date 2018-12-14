#! /usr/bin/env -S groovy
import java.nio.file.Files
import java.io.File
def n = Files.readAllLines(new File("14.in").toPath())[0].toInteger()
def nstr = n.toString()
def scores = [3, 7]
def elf1 = 0
def elf2 = 1

def res2 = -1
while (true) {
    def digits = (scores[elf1] + scores[elf2]).toString().split("").collect { it -> it.toInteger() }
    scores.addAll(digits)
    elf1 = (elf1 + scores[elf1] + 1) % scores.size()
    elf2 = (elf2 + scores[elf2] + 1) % scores.size()
    def size = scores.size()
    if (size > 6 && scores.subList(size - nstr.size() - 1, size - 1).join("") == nstr) {
        res2 = scores.size() - nstr.size() - 1
        if(size > n + 10) {
            break
        }
    }
}
println "Solution for part 1:"
println scores.subList(n, n + 10).join("")
println "Solution for part 2:"
println res2

// Solution part 1: 2107929416
// Solution part 2: 20307394
