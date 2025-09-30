import ast._
import util.InputNotExampleException

package main.validity

object ValidityChecker:
    def checkValidity(prog: Program): Program =
        if ASTInspector.progHasError(prog) then
            throw new InputNotExampleException("The Program AST has an Error!")

        // check the validity of the variables, converting variables to undefined variable error nodes when needed
        prog
