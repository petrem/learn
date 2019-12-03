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

function isAnagramOf(thing, other) {
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

    const thingCounts = countLetters(thing);
    const otherCounts = countLetters(other);
    return mapEquals(thingCounts, otherCounts);
};

function aclean(arr) {
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

let arr = ["nap", "teachers", "cheaters", "PAN", "ear", "era", "hectares"];

console.log( aclean(arr) );
