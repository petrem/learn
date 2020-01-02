export class Matrix {
  constructor(str) {
    this.matrix = str.replace("\r", "")
      .split("\n")
      .map((row) => row.split(" ").map(Number));
    this.n_rows = this.matrix.length;
    this.n_cols = this.matrix[0].length;
    if (!(this.matrix.every((row) => row.length == this.n_cols))) {
      throw new Error("Matrix should have same number of columns on each row");
    }
  }

  get rows() {
    return this.matrix;
  }

  get columns() {
    return zip(this.matrix);
  }
}

const zip = (rows) => rows[0].map((_, idx) => rows.map((row) => row[idx]));
