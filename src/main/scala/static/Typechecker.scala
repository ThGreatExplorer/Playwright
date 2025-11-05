package static

import ast._
import ast.ValidityErrNodes._

object Typechecker:

    def typecheckSys(s: CleanSystem): SystemWE = s match
        case System(modules, imports, progb) => 
            WE.Node(System(
                VCheckTLDups.moduleDupsModules(modules),
                imports.map(WE.Node(_)),
                ConverterToWE.progBlockToWE(progb)
            ))

   