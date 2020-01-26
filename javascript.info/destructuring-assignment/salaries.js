let salaries = {
  "John": 100,
  "Pete": 300,
  "Mary": 250
};

function topSalary(salaries) {
  let [name, salary] = Object.entries(salaries).reduce(
    (acc, e) => acc[1] >= e[1] ? acc : e,
    [null, -Infinity]
  );
  return name;
}

console.log(topSalary(salaries));
console.log(topSalary([]));
