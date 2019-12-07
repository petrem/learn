"use strict";

function mapEquals(m1, m2) {
    if (m1.size != m2.size)
        return false;
    for (let [key, value] of m1) {
        if (m2.get(key) != value)
            return false;
    }
    return true;
};

function countLetters(word) {
    const letterCounts = new Map();
    for (let letter of word) {
        if (letterCounts.has(letter)) {
            letterCounts.set(letter, letterCounts.get(letter) + 1);
        } else {
            letterCounts.set(letter, 1);
        }
    }
    return letterCounts;
}

function isAnagramOf(thing, other) {
    const thingCounts = countLetters(thing);
    const otherCounts = countLetters(other);
    return mapEquals(thingCounts, otherCounts);
};

function aclean_(arr) {
    const seen = new Set();
    function wasSeen(thing) {
        for (let word of seen) {
            if (isAnagramOf(thing, word)) {
                return true;
            }
        }
        return false;
    }
    for (let word of arr.map((w) => w.toLowerCase())) {
        if (!wasSeen(word)) {
            seen.add(word);
        }
    }
    return [...seen];
};

const zeroCounts = new Map([..."abcdefghijklmnopqrstuvwxyz"].map((l) => [l, 0]));

function gematria(word) {
    return [
        ...new Map([...zeroCounts, ...countLetters(word)])
            .entries()
    ].toString();
};


function aclean(arr) {
    const seen = new Map();
    for (let word of arr.map((w) => w.toLowerCase())) {
        let signature = gematria(word);
        if (!seen.has(signature)) {
            seen.set(signature, word);
        }
    }
    return [...seen.values()];
};
let arr = ["nap", "teachers", "cheaters", "PAN", "ear", "era", "hectares"];

console.log( aclean(arr) );
