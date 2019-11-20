"use strict";

// Write the function camelize(str) that changes dash-separated words like “my-short-string” into camel-cased “myShortString”.

// That is: removes all dashes, each word after dash becomes uppercased.

// Examples:

// camelize("background-color") == 'backgroundColor';
// camelize("list-style-image") == 'listStyleImage';
// camelize("-webkit-transition") == 'WebkitTransition';

// P.S. Hint: use split to split the string into an array, transform it and join back.


const capitalize = (str) => [str[0].toUpperCase(), str.substring(1)].join('');

const camelize = (str) => {
    const elements = str.split("-");
    const [first, rest] = [elements[0], elements.slice(1)];
    const head = first ? [first] : [];
    return head.concat(rest.filter((x) => x).map(capitalize)).join('');
};

["background-color"
 ,"list-style-image"
 ,"-webkit-transition"
 ,"foo--bar"
].forEach(
    (str) => console.log(camelize(str))
);
