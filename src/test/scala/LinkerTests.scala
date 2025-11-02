package test

import munit.FunSuite
import ast.ConverterToClean.systemToClean
import static.Parser
import static.{VCheckTLDups, VCheckMFPNameDups, VCheckUndefined}
import main.MainFuncs
import main.AssignmentRunner
import static.{SystemToClassLinker, ModuleDependency}

class LinkerTests extends FunSuite {

  def removeWhiteSpace(s: String): String = 
    s.replaceAllLiterally(" ", "")

  LinkerTests.validCases.zip(LinkerTests.expectedValidTestCaseResults).foreach{
  
    case (inputStr, (expectedDepGraph, expectedAST)) =>
      test(inputStr) {
        val inputSexp = MainFuncs.readSexp(inputStr)  
        val pipeRes =         
          for 
            parsedProg <- systemToClean(Parser.parseSys(inputSexp))
            vCheck1    <- systemToClean(VCheckTLDups.moduleDupsSys(parsedProg))
            vCheck2    <- systemToClean(VCheckMFPNameDups.mfpDupsSys(vCheck1))                
            validPr    <- systemToClean(VCheckUndefined.closedSystem(vCheck2))          
          yield        
            validPr

        pipeRes match
          case None => throw new Exception("Passed invalid test case for Linker")
          case Some(cleanSystem) =>
            val (baseModule, renamedSys) = SystemToClassLinker.renameClassesUsingDependencyGraph(cleanSystem)
            assertEquals(baseModule.toString().strip(), expectedDepGraph.strip())
            assertEquals(removeWhiteSpace(renamedSys.toString()), removeWhiteSpace(expectedAST.toString()))
      }
  } 
}

object LinkerTests {
  val expectedValidTestCaseResults = List(
  (
    "#Base@", 
    "System(List(),List(),ProgBlock(List(),List(),Num(0.0)))"
  ),
  (
  """#Base@
└── ModuleB
    └── ModuleA""", 
  """System(List(Module(ModuleA,List(),Class(ModuleA.ClassA,List(),List())),Module(ModuleB,List(ModuleA),Class(ModuleB.ClassB,List(),List()))),List(ModuleB),ProgBlock(List(),List(),Num(0.0)))"""
  ),
  ("""#Base@
└── ModuleA""", 
  """System(List(Module(ModuleA,List(),Class(ModuleA.ClassA,List(),List()))),List(ModuleA),ProgBlock(List(),List(),Num(0.0)))"""
  ),
  ("""#Base@
└── ModuleC
    ├── ModuleA
    └── ModuleB
        └── ModuleA (already shown)""", 
    """System(List(Module(ModuleA,List(),Class(ModuleA.ClassA,List(),List())),Module(ModuleB,List(ModuleA),Class(ModuleB.ClassB,List(),List())),Module(ModuleC,List(ModuleA,ModuleB),Class(ModuleC.ClassC,List(),List()))),List(ModuleC),ProgBlock(List(),List(),Num(0.0)))"""
  ),
  ("#Base@", 
  "System(List(),List(),ProgBlock(List(),List(),Num(0.0)))"
  ),
  ("#Base@", 
  "System(List(),List(),ProgBlock(List(),List(),Num(0.0)))"),
  ("""#Base@
├── ModuleB
│   └── ModuleA
├── ModuleD
│   └── ModuleC
│       └── ModuleB (already shown)
└── ModuleC (already shown)""", 
"""System(List(Module(ModuleA,List(),Class(ModuleA.ClassA,List(),List())),Module(ModuleB,List(ModuleA),Class(ModuleB.ClassB,List(),List())),Module(ModuleC,List(ModuleB),Class(ModuleC.ClassC,List(),List())),Module(ModuleD,List(ModuleC),Class(ModuleD.ClassD,List(),List()))),List(ModuleB,ModuleD,ModuleC),ProgBlock(List(),List(),Num(0.0)))"""
  ),
  ("""#Base@
├── ModuleA
└── ModuleB""", 
"""System(List(Module(ModuleA,List(),Class(ModuleA.ClassA,List(),List())),Module(ModuleB,List(),Class(ModuleB.ClassA,List(),List()))),List(ModuleA,ModuleB),ProgBlock(List(),List(),Num(0.0)))"""
  ),
  ("""#Base@
├── ModuleB
│   └── ModuleA
└── ModuleA (already shown)
  """,
  "System(List(Module(ModuleA,List(),Class(ModuleA.ClassA,List(),List())),Module(ModuleB,List(ModuleA),Class(ModuleB.ClassA,List(),List()))),List(ModuleB,ModuleA),ProgBlock(List(),List(),Num(0.0)))"
  ),
  ("""#Base@
├── ModuleAOne
└── ModuleBAOne
    ├── ModuleATwo
    └── ModuleAOne (already shown)""",
  "System(List(Module(ModuleAOne,List(),Class(ModuleAOne.A,List(),List())),Module(ModuleATwo,List(),Class(ModuleATwo.A,List(),List())),Module(ModuleBAOne,List(ModuleATwo,ModuleAOne),Class(ModuleBAOne.B,List(),List(Method(newA,List(),ProgBlock(List(),List(),NewInstance(ModuleAOne.A,List()))))))),List(ModuleAOne,ModuleBAOne),ProgBlock(List(Decl(b,NewInstance(ModuleBAOne.B,List())),Decl(aOne,CallMethod(b,newA,List())),Decl(aOneO,NewInstance(ModuleAOne.A,List()))),List(),BinOpExpr(aOneO,Equals,aOne)))")
)

  val validCases = List(
    """
    (
      (module ModuleA (class ClassA ()))
      0.0
    )
    """,
    """
    (
      (module ModuleA (class ClassA ()))
      (module ModuleB (import ModuleA) (class ClassB ()))
      (import ModuleB)
      0.0
    )
    """,
    """
    (
      (module ModuleA (class ClassA ()))
      (import ModuleA)
      0.0
    )
    """,
    """
    (
      (module ModuleA (class ClassA ()))
      (module ModuleB (import ModuleA) (class ClassB ()))
      (module ModuleC (import ModuleA) (import ModuleB) (class ClassC ()))
      (import ModuleC)
      0.0
    )
    """,
    """
    (
      (module ModuleA (class ClassA ()))
      (module ModuleB (class ClassA ()))
      (module ModuleC (import ModuleA) (import ModuleB) (class ClassA ()))
      0.0
    )
    """,
    """
    (
      (module ModuleX (class ClassX ()))
      (module ModuleA (import ModuleX) (class ClassA ()))
      (module ModuleB (import ModuleX) (class ClassB ()))
      (module ModuleC (import ModuleA) (import ModuleB) (class ClassC ()))
      0.0
    )
    """,
    """
    (
      (module ModuleA (class ClassA ()))
      (module ModuleB (import ModuleA) (class ClassB ()))
      (module ModuleC (import ModuleB) (class ClassC ()))
      (module ModuleD (import ModuleC) (class ClassD ()))
      (import ModuleB)
      (import ModuleD)
      (import ModuleC)
      0.0
    )
    """,
    """
    (
      (module ModuleA (class ClassA ()))
      (module ModuleB (class ClassA ()))
      (import ModuleA)
      (import ModuleB)
      0.0
    )
    """,
    """
    (
      (module ModuleA (class ClassA ()))
      (module ModuleB (import ModuleA) (class ClassA ()))
      (import ModuleB)
      (import ModuleA)
      0.0
    )
    """,
    """
    (
      (module ModuleAOne (class A ()))
      (module ModuleATwo (class A ()))
      (module ModuleBATwo (import ModuleAOne) (import ModuleATwo) (class B () 
        (method newA ()
          (new A ())
        )
      ))
      (module ModuleBAOne (import ModuleATwo) (import ModuleAOne) (class B () 
        (method newA ()
          (new A ())
        )
      ))
      (import ModuleAOne)
      (import ModuleBAOne)
      (def b (new B ()))
      (def aOne (b --> newA ()))
      (def aOneO (new A ()))
      (aOneO == aOne)
    )
    """
  )
}