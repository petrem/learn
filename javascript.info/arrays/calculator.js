"use strict";

function Calculator () {
    return {
        operators: {
            "+": (x, y) => (+x) + (+y),
            "-": (x, y) => (+x) - (+y),
        },
        calculate: function (str) {
            let [x, op, y] = str.split(" ");
            return this.operators[op](x, y);
        },
        addMethod: function (name, func) {
            this.operators[name] = func;
        },
    };
};

let calc = new Calculator();

calc.addMethod("**", (x, y) => x ** y);

console.log(calc.calculate("3 ** 2"));
