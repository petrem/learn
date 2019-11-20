"use strict";

var foo = {};

function A() {
    return foo;
};

function B() {
    return foo;
}

let a = new A();
let b = new B();

console.log( a == b ); // true
