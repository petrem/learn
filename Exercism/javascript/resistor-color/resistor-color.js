export const colorCode = (color) => {
    if (typeof(color) !== "string")
        return null;
    let value = COLORS.findIndex((x) => {return x === color.toLowerCase()});
    return value >=0 ? value : null;
};

export const COLORS = [
    "black"
    , "brown"
    , "red"
    , "orange"
    , "yellow"
    , "green"
    , "blue"
    , "violet"
    , "grey"
    , "white"
];
