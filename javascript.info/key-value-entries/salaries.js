function sumSalaries(salaries) {
    let sum = 0;
    for (salary of Object.values(salaries)) {
        sum += salary;
    }
    return sum;
};

function sumSalaries2(salaries) {
    return Object.values(salaries).reduce((a, b) => a + b, 0);
};

let salaries = {
  "John": 100,
  "Pete": 300,
  "Mary": 250
};

console.log(sumSalaries(salaries), sumSalaries2(salaries));
