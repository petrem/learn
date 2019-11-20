export const isPalindrome = (thing) => {
    return [...thing].reverse().join("") == thing;
};
