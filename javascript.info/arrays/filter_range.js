"use strict";
// Write a function filterRange(arr, a, b) that gets an array arr, looks for elements between a and b in it and returns an array of them.
// The function should not modify the array. It should return the new array.
// For instance:
// let arr = [5, 3, 8, 1];
// let filtered = filterRange(arr, 1, 4);
// alert( filtered ); // 3,1 (matching values)
// alert( arr ); // 5,3,8,1 (not modified)

const filterRange = (arr, min, max) => arr.filter((x) => x >= min && x <= max);

// Write a function filterRangeInPlace(arr, a, b) that gets an array arr and removes from it all values except those that are between a and b. The test is: a ≤ arr[i] ≤ b.
// The function should only modify the array. It should not return anything.
// For instance:
// let arr = [5, 3, 8, 1];
// filterRangeInPlace(arr, 1, 4); // removed the numbers except from 1 to 4
// alert( arr ); // [3, 1]

const filterRangeInPlace = (arr, min, max) => {
    arr.forEach((item, index, array) => {
        if (item < min || item > max)
            array.splice(index, 1);
    });
};


let arr = [5, 3, 8, 1];
filterRangeInPlace(arr, 1, 4);
console.log(arr);
