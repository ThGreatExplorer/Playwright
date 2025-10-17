package static

import ast._
import ast.ValidityErrNodes._
import util.InputNotExampleException
import util.UnreachablePatternMatch

object VCheckClassDups:

    def classDupsProg(p: CleanProgram): ProgramWE = p match
        case Program(classes, decls, stmts, expr) => 
            WE.Node(Program(
                classDupsClasses(classes),
                decls.map(ConverterToWE.declToWE),
                stmts.map(ConverterToWE.stmtToWE),
                ConverterToWE.exprToWE(expr)
            ))

    def classDupsClasses(clss: List[CleanClass]): List[ClassWE] =  

        def processClass(classNameNode : NameWE, fields : List[CleanName], methods : List[CleanMethod]) : ClassWE = 
            WE.Node(Class(
                classNameNode,
                fields.map(ConverterToWE.nameToWE),
                methods.map(ConverterToWE.methodToWE)
            ))

        def loop(clssRem: List[CleanClass], clssSoFar: List[ClassWE], namesSoFar: Set[String]) : List[ClassWE] = 
            clssRem match
                case Nil => clssSoFar.reverse
                    
                case Class(Name(name), fields, methods) :: tail => 
                    val classNameNode = 
                        if namesSoFar.contains(name) then 
                            WE.Err(DuplicateClassName) 
                        else 
                            WE.Node(Name(name))

                    val processedClasses =
                        processClass(classNameNode, fields, methods) :: clssSoFar

                    loop(tail, processedClasses, namesSoFar.incl(name))
                
        loop(clss, Nil, Set())

