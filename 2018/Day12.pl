#! /usr/bin/env perl
use List::Util qw[min max reduce];

open my $f, '<', "12.in";
my $data = do { local $/; <$f> };
my @lines = split /\n/, $data;

@lines[0] =~ /^initial state: ([#\.]+)/;
my %init;
foreach (0..length($1) - 1) {
    $init{$_} = substr($1, $_, 1);
}

$init{-1} = ".";
$init{-2} = ".";
$init{length($1)} = ".";
$init{length($1) + 1} = ".";

my @ruleLines = splice (@lines, 2);

my %rules;
foreach(@ruleLines) {
    $_ =~ /([#\.]{5}) => ([#\.])/;
    $rules{$1} = $2;
}

sub gen(%rules, %init) {
    local $min = min(keys(%init));
    local $max = max(keys(%init));
    $init{$min - 1} = ".";
    $init{$min - 2} = ".";
    $init{$min - 3} = ".";
    $init{$min - 4} = ".";
    $init{$min - 5} = ".";
    $init{$max + 1} = ".";
    $init{$max + 2} = ".";
    $init{$max + 3} = ".";
    $init{$max + 4} = ".";
    $init{$max + 5} = ".";

    local %new;
    for ($min-3..$max+3) {
        local $r = $rules{$init{$_ - 2} . $init{$_ - 1} . $init{$_} . $init{$_ + 1} . $init{$_ + 2}};
        $new{$_} = $r ? $r : ".";
    }
    $min = min(keys(%init));
    $max = max(keys(%init));
    return %new;
}

my $prev = 0;
foreach (1..100) {
    @keys = sort { $a <=> $b } (keys %init);
    @vals = map($init{$_}, @keys);
    local $v = reduce { $a + (($init{$b} eq "#") ? $b : 0)} 0, (keys %init);
    print $_ . ": " . ($v - $prev) . " - " . $v . "\n";
    $prev = $v;
    %init = gen(%rules, %init);
    if ($_ == 21) {
        print "Solution for part 1:\n";
        print $v . "\n";
    }
}

# Solution part 1: 6201
# Solution part 2: 
#   Pattern starting at #93 => 18321 + 186 * (50000000000 - 93) = 9300000001023
