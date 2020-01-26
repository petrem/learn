"use strict";

const countProperties = (obj) => Object.keys(obj).length;

let user = {
  name: 'John',
  age: 30
};

console.log(countProperties(user));
