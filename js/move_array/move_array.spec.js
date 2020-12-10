import { arrange } from './move_array';

describe('Move element in array', () => {
  describe("Given an array, a from and to positions, return a new array with element moved.", () => {
    // from == to
    test('same position: start to start', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 0, 0);
      expect(magics).toEqual(before);
      expect(magics).not.toBe(before);
    });

    test('same position: end to end', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 4, 4);
      expect(magics).toEqual(before);
      expect(magics).not.toBe(before);
    });

    test('same position: in the middle', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 2, 2);
      expect(magics).toEqual(before);
      expect(magics).not.toBe(before);
    });

    test('same position: end to end, negative `to`', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 4, -1);
      expect(magics).toEqual(before);
      expect(magics).not.toBe(before);
    });

    test('same position: in the middle, negative `to`', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 2, -3);
      expect(magics).toEqual(before);
      expect(magics).not.toBe(before);
    });

    test('same position: in the middle, negative `from`', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, -2, 3);
      expect(magics).toEqual(before);
      expect(magics).not.toBe(before);
    });

    test('same position: in the middle, negative both', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, -2, -2);
      expect(magics).toEqual(before);
      expect(magics).not.toBe(before);
    });

    // from + 1 == to
    test('consecutive, at start', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 0, 1);
      expect(magics).toEqual(['❤ 9', '❤ A', '❤ 3', '❤ 6', '♣ A',]);
      expect(magics).not.toBe(before);
    });


    test('consecutive, in middle', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 1, 2);
      expect(magics).toEqual(['❤ A', '❤ 3', '❤ 9', '❤ 6', '♣ A',]);
      expect(magics).not.toBe(before);
    });

    test('consecutive, at end)', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 3, 4);
      expect(magics).toEqual(['❤ A', '❤ 9', '❤ 3', '♣ A', '❤ 6',]);
      expect(magics).not.toBe(before);
    });

    // from + 1 < to

    test('from start to end', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 0, 4);
      expect(magics).toEqual(['❤ 9', '❤ 3', '❤ 6', '♣ A','❤ A',]);
      expect(magics).not.toBe(before);
    });

    test('from start to middle', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 0, 3);
      expect(magics).toEqual(['❤ 9', '❤ 3', '❤ 6',  '❤ A',  '♣ A',]);
      expect(magics).not.toBe(before);
    });

    test('from middle to middle', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 1, 3);
      expect(magics).toEqual(['❤ A', '❤ 3', '❤ 6', '❤ 9', '♣ A',]);
      expect(magics).not.toBe(before);
    });

    test('from middle to end', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 2, 4);
      expect(magics).toEqual(['❤ A', '❤ 9', '❤ 6', '♣ A', '❤ 3',]);
      expect(magics).not.toBe(before);
    });

    // from == to + 1
    test('reversed consecutive, at start', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 1, 0);
      expect(magics).toEqual(['❤ 9', '❤ A', '❤ 3', '❤ 6', '♣ A',]);
      expect(magics).not.toBe(before);
    });


    test('reversed consecutive, at end)', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 4, 3);
      expect(magics).toEqual(['❤ A', '❤ 9', '❤ 3', '♣ A', '❤ 6',]);
      expect(magics).not.toBe(before);
    });

    test('reversed consecutive, in middle)', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 3, 2);
      expect(magics).toEqual(['❤ A', '❤ 9','❤ 6',  '❤ 3', '♣ A',]);
      expect(magics).not.toBe(before);
    });

    // from + 1 > to

    test('from end to start', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 4, 0);
      expect(magics).toEqual(['♣ A', '❤ A', '❤ 9', '❤ 3', '❤ 6',]);
      expect(magics).not.toBe(before);
    });

    test('from end to middle', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 4, 2);
      expect(magics).toEqual(['❤ A', '❤ 9', '♣ A', '❤ 3', '❤ 6',]);
      expect(magics).not.toBe(before);
    });

    test('from middle to middle', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 3, 1);
      expect(magics).toEqual(['❤ A', '❤ 6', '❤ 9', '❤ 3', '♣ A',]);
      expect(magics).not.toBe(before);
    });

    test('from middle to start', () => {
      const before = ['❤ A', '❤ 9', '❤ 3', '❤ 6', '♣ A',];
      const magics = arrange(before, 2, 0);
      expect(magics).toEqual(['❤ 3', '❤ A', '❤ 9', '❤ 6', '♣ A',]);
      expect(magics).not.toBe(before);
    });
  });
});
