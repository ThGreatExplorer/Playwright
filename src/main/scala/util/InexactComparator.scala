package util

import ast.NumVal

private val EPSILON = 0.0000000001

extension (value : Double) {

    def =~=(that:Double) : Boolean =
        // this is not a good way to do a fuzzy comparison. You should have both relative
        // and absolute precision. But for an example like this it should suffice.
        (value - that).abs < EPSILON

    def isZero() : Boolean = 
        value =~= 0.0
}

// object InexactComparator:
//     private val EPSILON = 0.0000000001

//     def numValIsZero(v : NumVal) : Boolean =
//         numValsAreEqual(v, 0.0)

//     def numValsAreEqual(v1 : NumVal, v2 : NumVal) : Boolean = 
//         val diff = math.abs(v1 - v2)
//         diff < EPSILON