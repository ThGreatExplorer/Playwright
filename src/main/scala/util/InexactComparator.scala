package util

import ast.NumVal

object InexactComparator:
    private val EPSILON = 0.0000000001

    def numValIsZero(v : NumVal) : Boolean =
        numValsAreEqual(v, 0.0)

    def numValsAreEqual(v1 : NumVal, v2 : NumVal) : Boolean = 
        val diff = math.abs(v1 - v2)
        diff < EPSILON