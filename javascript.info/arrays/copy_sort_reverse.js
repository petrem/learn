"use strict";

const copySortReverse = (arr) => arr.copyWithin().sort((a, b) => b - a);

let arr = [2,5,3,8,1];
console.log(copySortReverse(arr));
