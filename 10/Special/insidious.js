import { Helper } from './helper.ts';

class Insidious {
    constructor() {}
    
    violation() {
        const o = new Helper();
        const x = o;
        const value = o.faveNumber(x);
        console.log(value);
        return value instanceof Helper;
    }
}

export { Insidious };