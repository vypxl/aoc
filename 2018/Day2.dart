#! /usr/bin/env dart
import 'dart:io';

main() {
  var lines = new File('input2.txt').readAsStringSync().split("\n")..sort();
  print("Solution for part 1:");
  print(
    [2, 3].fold(1, (a, count) => 
      (a as int) * lines.where(
          (l) => l.runes.fold(false, 
            (a, r) => a || (l.runes.where((x) => x == r).length == count)
          )
        ).length
      )
    );
  
  print("Solution for part 2:");
  for (var i = 0; i < lines.length - 1; i++) {
    var chars = [];
    for (var j = 0; j < lines[0].length; j++) {
      if (lines[i][j] == lines[i + 1][j]) chars.add(lines[i][j]);
    }
    if (chars.length == lines[i].length - 1) {
      print(chars.join());
      break;
    }
  }
}

// Solution 1: 7105
// Solution 2: omlvgdokxfncvqyersasjziup
