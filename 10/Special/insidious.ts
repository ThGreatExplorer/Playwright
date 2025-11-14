import { Insidious } from './insidious.js';


declare module './insidious.js' {
    export class Insidious {
        constructor();
        violation(): boolean;
    }
}

// Program execution
let x: number = 1.0;
const o = new Insidious();
const result = o.violation();
console.log(result)

/**
 * ts-node 10/Special/insidious.ts
Helper {} // should be a number!
true // should be false
 */