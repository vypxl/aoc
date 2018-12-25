#! /usr/bin/env node

function group(from) {
    return {
        n: parseInt(from[0]),
        hp: parseInt(from[1]),
        immunities: ((from[2] ? from[2] : '') + (from[4] ? from[4] : "")).split(', ').filter(x => x),
        weaknesses: (from[3] ? from[3] : '').split(', ').filter(x => x),
        atk: parseInt(from[5]),
        kind: from[6],
        init: parseInt(from[7]),

        side: -1,
        target: null,
        targeted: false,

        eff: function () { return this.n * this.atk },
        damageTo: function (other) { 
            if (other.immunities.includes(this.kind)) return 0;
            let mult = other.weaknesses.includes(this.kind) ? 2 : 1;
            return this.n * this.atk * mult;
        },
        attack: function () {
            this.target.n -= Math.floor(this.damageTo(this.target) / this.target.hp);
            this.target.targeted = false;
            this.target = null;
        },
    };
}

function parse(data, boost) {
    const regex = /(\d+) units each with (\d+) hit points (?:\((?:immune to ([\w, ]+))?;? ?(?:weak to ([\w, ]+))?;? ?(?:immune to ([\w, ]+))?\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)/;
    let [imm, inf] = data.split('\n\n')
        .map(xs => xs.split('\n').filter(l => /\d/.test(l)))
        .map(xs => xs.map(l => regex.exec(l).slice(1, 9)))
        .map(xs => xs.map(group));
    return [imm.map(x => ({...x, atk: x.atk + boost, side: 1})), inf.map(x => ({...x, side: 2}))].flat();
}

function targetSelect(groups) {
    groups.sort((a, b) => (a.eff() === b.eff()) ? b.init - a.init : b.eff() - a.eff());
    for (g of groups) {
        let target = groups
        .filter(x => !x.targeted && g.side != x.side)
        .reduce((acc, n) => {
            if (acc == null) return n;
            let da = g.damageTo(acc);
            let dn = g.damageTo(n);
            if (da < dn) return n;
            else if (da > dn) return acc;
            let ea = acc.eff();
            let en = n.eff();
            if (ea < en) return n;
            if (ea > en) return acc;
            if (acc.init < n.init) return n;
            else return acc;
        }, null);
        if (target === null || g.damageTo(target) == 0 || target.targeted || g.side == target.side) continue;

        target.targeted = true;
        g.target = target;
    }
    return groups;
}

function attack(groups) {
    groups.sort((a, b) => b.init - a.init);
    for (g of groups) {
        if (g.n < 1 || g.target === null) continue;
        g.attack();
    }
    return groups.filter(g => g.n > 0);
}

function battle(data, boost) {
    let groups = parse(data, boost);
    
    let rounds = 0;
    while (!(groups.every(g => g.side === 1) || groups.every(g => g.side === 2))) {
        groups = attack(targetSelect(groups));
        // Prevent infinite looping
        if (rounds > 2000) return ['Tie', -1];
        rounds++;
    }
    
    return [groups[0].side === 1 ? 'Immune System' : 'Infection', groups.reduce((a, g) => a + g.n, 0)]
}

const f = require('fs').readFileSync('24.in').toString();

console.log('Solution for part 1:');
console.log(battle(f, 0)[1]);
console.log('Solution for part 2:');
b = 0;
while(true) {
    [winner, outcome] = battle(f, b);
    if(winner == 'Immune System') {
        console.log(outcome);
        break;
    }
    b++;
}
// Solution part 1: 25524
// Solution part 2: 4837
