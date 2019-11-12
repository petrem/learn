export const isPangram = (thing) => {
    let checklist = new Set("abcdefghijklmnopqrstuvwxyz");
    [...thing.toLowerCase()].forEach( (item, index, array) => { checklist.delete(item); } );
    return checklist.size == 0;
};
