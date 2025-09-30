## Challenge Questions

This file contains answers for extra-credit challenge questions presented throughout the course. 

### Assignment 3 - Order of Operation equivalence (n * 3/100 vs 3/100 * n)

 Q: How would the problem that is mentioned in the dialog between Eli Barzilay and the AI 
 affect the students in this course? 
 A: We will inevitably have to deal with floating point numbers in our software development
 careers, but few people actually know how inexact numbers are handled by our machines.
 The problem highlighted by Eli Barzilay is insidious because the surface syntax by itself 
 does not indicate that either expression is better than other, but there is a tangible
 difference.
 
 Q: How does the design of Bare Bones avoid the problem?
 A: BareBones syntax is extremely restrictive. In particular, the grammar for evalutaing 
 expressions only allows *variables* in operand position, and not expressions. This means 
 that non-nesting is enforced at the grammar level, and there is only one way we can express
 calculations.
  