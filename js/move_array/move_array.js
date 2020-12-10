// @ts-check

/**
 * Re-arranges the input array by moving an item from one position to another position.
 * Does not mutate the input array.
 *
 * @template T
 *
 * @param {T[]} array the input array
 * @param {number} from the position to move from
 * @param {number} to the position to move to
 *
 * @returns {T[]}
 */
export function arrange(array, from, to) {
  from = abs_index(from, array.length);
  to = abs_index(to, array.length);
  const card = array.slice(from, from+1);
  if (from < to) {
    let slices = [
      array.slice(0, from),
      array.slice(from+1, to+1),
      array.slice(from, from+1), // the moved card
      array.slice(to+1)
    ];
    console.error(slices);
    return [].concat(...slices);
  }
  else if (from > to) {
    let slices = [
      array.slice(0, to),
      array.slice(from, from+1),  // the moved card
      array.slice(to, from),
      array.slice(from+1)
    ];
    return [].concat(...slices);
  } else {
    return array.slice();
  }
};

const abs_index = (i, n) => (i + n ) % n;

/*
(0, from), (from+1, to+1), (from, from+1), (to+1, )
(0, to), (from, from+1), (to, from), (from+1, )

(0, m), (m+1, M+1), (m, m+1), (M+1, )
(0, m), (M, M+1), (m, M), (M+1, )

*/
