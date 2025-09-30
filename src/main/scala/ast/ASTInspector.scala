package ast

object ASTInspector:
    /** Walks the AST and determines if any of the nodes contain an error. 
      * Short-circuits.
      * 
      * @param p Program AST
      * @return true if the AST contains error nodes
      */
    def progHasError(p: Program): Boolean = p match
        // Using an iterative approach to avoid stack overflow as exists() is 
        // recursively implemented on Lists
        case Program.Prog(stmts, expr) => 
            stmts.iterator.exists(hasError) || hasError(expr)
        case Program.Err(_) => true

    def hasError(s: Statement): Boolean = s match
        case Statement.Assign(lhs, rhs) => hasError(lhs) || hasError(rhs)
        case Statement.Ifelse(guard, tbranch, ebranch) => 
            hasError(guard) || hasError(tbranch) || hasError(ebranch)
        case Statement.While(guard, body) => hasError(guard) || hasError(body)
        case Statement.Err(_) => true

    def hasError(b: Block): Boolean = b match
        case Block.One(stmt) => hasError(stmt)
        // Similarly, using iterator
        case Block.Many(stmts) => stmts.iterator.exists(hasError)
        case Block.Err(_) => true
    
    def hasError(e: Expression): Boolean = e match
        case Expression.Num(_) | Expression.Var(_) => false
        case Expression.Add(lhs, rhs) => hasError(lhs) || hasError(rhs)
        case Expression.Div(lhs, rhs) => hasError(lhs) || hasError(rhs)
        case Expression.Equals(lhs, rhs) => hasError(lhs) || hasError(rhs)
        case Expression.Err(_) => true
