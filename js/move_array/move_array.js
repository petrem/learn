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
  [from, to] = [from, to].map(abs_index.bind(null, array.length));
  // creates at least 3 objects that aren't necessary
  // maybe [from, to].sort() is faster in this case?
  const [min, max] = from < to ? [from, to] : [to, from];
  const array_slicer = Array.prototype.slice.bind(array);
  return [].concat(
    ...[].concat(
      [[0, min]],
      from < to ? [[min+1, max+1], [min, min+1]] : [[max, max+1], [min, max]],
      [[max+1]]
    ).map(slice => array_slicer(...slice))
  );
};


export function rearrange(array, from, to) {
  const card = array.splice(from, 1);
  array.splice(to < 0 ? to+1 : to, 0, ...card);
  return array;
};


const abs_index = (n, i) => (i + n ) % n;
