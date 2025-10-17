package static

import ast._
import ast.ValidityErrNodes._

object VCheckMethodFieldParamDups:

    def mfpDupsProg(p: CleanProgram): ProgramWE = p match
        case CleanProgram(clss, decls, stmts, expr) =>
            WE.Node(Program(
                clss.map(classDupsMethodFieldParams),
                decls.map(ConverterToWE.declToWE),
                stmts.map(ConverterToWE.stmtToWE),
                ConverterToWE.exprToWE(expr)
            ))

    def classDupsMethodFieldParams(clss: CleanClass): ClassWE = clss match
        case Class(Name(cname), fields, methods) =>
            val fieldDups  = processFieldDups(fields)
            val methodDups = processMethodDups(methods)
            WE.Node(Class(
                WE.Node(Name(cname)),
                fieldDups,
                methodDups
            ))

    def processFieldDups(fields: List[CleanName]): List[NameWE] =

        def loop(fieldsRemaining: List[CleanName], fieldsSoFar: List[NameWE], uniqueFieldNames: Set[String]): List[NameWE] = fieldsRemaining match
            case Nil => fieldsSoFar.reverse
            case (Name(field) :: rest) =>
                val newField: NameWE = if uniqueFieldNames.contains(field) then 
                    WE.Err(DuplicateFieldName)
                else 
                    WE.Node(Name(field))
                loop(rest, newField :: fieldsSoFar, uniqueFieldNames.incl(field))

        loop(fields, Nil, Set())

    def processMethodDups(methods: List[CleanMethod]): List[MethodWE] =

        def loop(methodsRemaining: List[CleanMethod], methodsSoFar: List[MethodWE], uniqueMethodName: Set[String]): List[MethodWE] = methodsRemaining match
            case Nil => methodsSoFar.reverse
            case (Method(Name(name), params, decls, stmts, expr) :: rest) =>
                val newMethodName: NameWE =
                    if uniqueMethodName.contains(name) then 
                        WE.Err(DuplicateMethod)
                    else 
                        WE.Node(Name(name))

                val paramDups = processParamDups(params)
                val newMethod = WE.Node(Method(
                    newMethodName,
                    paramDups,
                    decls.map(ConverterToWE.declToWE),
                    stmts.map(ConverterToWE.stmtToWE),
                    ConverterToWE.exprToWE(expr)
                ))
                loop(rest, newMethod :: methodsSoFar, uniqueMethodName.incl(name))

        loop(methods, Nil, Set())

    def processParamDups(params: List[CleanName]): List[NameWE] =

        def loop(paramsRemaining: List[CleanName], methodsSoFar: List[NameWE], uniqueParamName: Set[String]): List[NameWE] = paramsRemaining match
            case Nil => methodsSoFar.reverse
            case (CleanName(x) :: rest) =>
                val newParam: NameWE = if uniqueParamName.contains(x) then 
                    WE.Err(DuplicateParamName)
                else 
                    WE.Node(Name(x))
                loop(rest, newParam :: methodsSoFar, uniqueParamName.incl(x))

        loop(params, Nil, Set())
