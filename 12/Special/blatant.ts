import { Blatant } from './blatant.js';

declare module './blatant.js' {
    export class Blatant {
        violation(): number; // doesn't actually return number but object
    }
}

const bla = new Blatant();
const result = bla.violation();
const calculation = result + 5;
console.log(calculation);

/*
✗ ts-node blatant.ts
[object Object]5
*/