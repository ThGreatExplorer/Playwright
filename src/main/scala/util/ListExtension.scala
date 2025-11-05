package util

import ast._
import sexprs.SExprs._

extension [A](list: List[A])
    /** 
     * Transforms `List[A]` into `List[B]` wrapped in an Option. 
     * 
     * During iteration, the method calls `f` on each element of the list.
     * If all applications succeed (return `Some` value), it returns `Some`
     * containing the list of all results. If any application fails (returns `None`),
     * the entire traversal short-circuits and returns `None`.
     * 
     * Extension method for generic List[A]. 
     * 
     * Inspired by monadic traverse operation from the cats library: 
     * https://www.javadoc.io/doc/org.typelevel/cats-docs_2.13/latest/cats/Traverse.html
     *
     * @param B the result type of the transformation
     * @param f the function to apply to each element, which may fail, returning `None`
     * @return `Some(List[B])` if all transformations succeed, `None` if any fails
     */
    def traverse[B](f: A => Option[B]): Option[List[B]] =
        list.foldRight(Some(Nil): Option[List[B]]) { (a, acc) =>
            for
                bs <- acc
                b <- f(a)
            yield 
                b :: bs
        }

extension (names : List[String])
    /**
      * Heavily utlized in Class validity checking for confirming that lists of
      * names do not contain duplicates. If a name is a duplicate, we will replace
      * it with a relevant error node. 
      * 
      * Traverses a list of Clean names, keeping track of unqiue names already seen
      * and wraps them into WE constructor. If a name is unique so far, we create a
      * WE.Node, otherwise we create WE.Err with the provided fallback validity error node  
      *
      * @param names List of String nodes that we expect to be unique
      * @param errNode ValidityErrNode that will replace the duplicate String node 
      * @return List of WE String nodes to be packaged back into the AST
      */
    def identifyNameDupsWErr(errNode : ValidityErrNodes) : List[WE[String]] = 

        def loop(
            remainingNames: List[Clean[String]], 
            namesSoFar: List[WE[String]], 
            uniqueNames: Set[String]
        ): List[WE[String]] = remainingNames match

            case Nil => namesSoFar.reverse

            case (name :: rest) =>
                val nameWE: WE[String] =
                    if uniqueNames.contains(name) then 
                        WE.Err(errNode)
                    else 
                        WE.Node(name)

                loop(rest, nameWE :: namesSoFar, uniqueNames.incl(name))

        loop(names, Nil, Set())

/******************************************************************************
  List extensions for AST Node field lists 
 *****************************************************************************/

// Type utils

extension (ftypes : List[FieldType[Clean]])
    def getFTypeNames : List[String] = ftypes.map{ 
        case FieldType(fname, _) => fname 
    }

extension (mtypes : List[MethodType[Clean]])
    def getMTypeNames : List[String] = mtypes.map{ 
        case MethodType(mname, _, _) => mname 
    }

// Module utils

extension (modules : List[Module[Clean]])
    def getMDNames : List[String] = modules.map{ 
        case Module(mname, _, _, _) => mname 
    }

extension (modules : List[Module[Clean]])
    def getClasses : List[Class[Clean]] = modules.map{ 
        case Module(_, _, clas, _) => clas 
    }

// Class utils

extension (clss : List[Class[Clean]])
    def getCNames : List[String] = clss.map{ 
        case Class(cname, _, _) => cname 
    }

// Method utils

extension (methods : List[Method[Clean]])
    def getMNames : List[String] = methods.map{ 
        case Method(mname, _, _) => mname 
    }