package cesk

import ast._
import scala.collection.immutable.Map
import util.UnreachablePatternMatch

final case class CESKState(
  control : Control, 
  env     : Env,
  store   : Store, 
  kont    : KontStack
): 
  /**
  * Returns true if the given state is a final state. A final state is either an Error state or 
  * a Success state with Value in Control and an empty KontStack stack.
  *
  * @param state the current state of the CESK machine
  * @return true if the state is final, false otherwise
  */
  def isFinal() : Boolean =
    control match { 
      case Control.Err(_) => kont.isEmpty
      case Control.Value(_) => kont.isEmpty
      case _ => false
    }

  def lookupVar(x : String) : NumVal = 
    val loc = env.getLoc(x) 
    store.getVal(loc) 

object CESKState:
  /**
    * Helper method to construct an error state
    *
    * @param e the RuntimeError
    * @return a CESKState in the error state
    */
  def constructErrorState(e: RuntimeError) : CESKState =
    CESKState(
      control = Control.Err(e),
      env = Env(),
      store = Store(),
      kont = KontStack()
    )

/******************************************************************************
  Below we define interfaces and type aliases for CESK registers. 
  
  Most of these are thin wrappers around existing Scala data sctructures, but 
  this implementation allows us to encapsulate our implementation details and 
  swap out the underlying structure if desired.

 *****************************************************************************/

// Inexact numbers that are understood to be Values in our CESK machine.
type NumVal = Double

// Locations corresponding to Values in Store. We want to have a (practically)
// infinite set of possible locations such that we can always produce a fresh
// location. Iterator built on Integers fits our needs nicely. 
type Loc = Integer

// Control Register is an enum
enum Control:
  case Expr(e : CleanExpr)
  case Value(n : NumVal)
  case Err(e : RuntimeError)
  case Search

// Env Register is a Map from Strings to Locations with the following interface
trait Env:
  def updatedEnv(x : String, loc : Loc) : Env
  def getLoc(x : String) : Loc

object Env:

  // Constructor that returns instances with the Env trait without actually
  // exposing internal imlementation of the underlying private class
  def apply(entries: (String, Loc)*): Env = 
    new MapEnv(Map(entries*))

  // Actual class we instantiate that is hidden from users
  private class MapEnv(underlying : Map[String, Loc]) extends Env {
    
    def updatedEnv(x : String, loc : Loc) : Env =
      new MapEnv(underlying.updated(x, loc))

    def getLoc(x : String) : Loc =
      underlying.get(x) match
        case Some(loc) => loc
        case None =>   
          // Technically unnecessary throw but it will help us catch env errors early
          throw new UnreachablePatternMatch(
            "Should never happen: variable " + x + " not found in environment"
          )    
  }

// Store Register is a Map from Locations to NumVals with the following interface
trait Store:
  def insertValAtNewLoc(n: NumVal) : (Store, Loc)
  def updatedStore(loc: Loc, n: NumVal) : Store
  def getVal(loc : Loc) : NumVal

object Store:

  def apply(entries: (Loc, NumVal)*): Store = 
    new MapStore(Map(entries*))

  // Private iterator that will produce fresh locations 
  // Is only accessed from within the private MapStore class
  private val locactionGenerator = Iterator.from(0)
  private def freshLoc() : Loc = locactionGenerator.next()

  private class MapStore(underlying : Map[Loc, NumVal]) extends Store {

    def insertValAtNewLoc(n : NumVal) : (Store, Loc) =
      val loc = freshLoc();
      val newStore = new MapStore(underlying.updated(loc, n))
      (newStore, loc)

    def updatedStore(loc : Loc, n : NumVal) : Store =
      new MapStore(underlying.updated(loc, n))

    def getVal(loc : Loc) : NumVal =
      underlying.get(loc) match
        case Some(n) => n
        case None =>   
          throw new UnreachablePatternMatch(
            "Should never happen: location " + loc + " not found in store"
          )    
  }

// Clousre is a tuple of a ProgramFrame and an Env snapshot right before we 
// began executing the current ProgramFrame (aka surrounding context)
type Closure = (ProgFrame, Env)

// ProgramFrame represents remaining instructions of either the TL program
// or a nested block; if the frame corresponds to a Block, expr is initialized
// with a Unit ()
case class ProgFrame(
  decls: List[CleanDecl], 
  stmts: List[CleanStmt | CleanBlock], 
  expr: CleanExpr | Unit
)

// Kont Register is a Stack of Closures with the following interface
trait KontStack:
  def push(clo : Closure) : KontStack
  def pop : KontStack
  def updateTopProgFrame(newProgFrame : ProgFrame) : KontStack
  def isEmpty : Boolean 
  def top : Closure
  def topProgFrame : ProgFrame
  def topEnv : Env 

object KontStack:

  def apply(entries: Closure*) : KontStack = new StackifiedList(List(entries*))

  // Special constructor called from load 
  def constructWithTL(initProg : CleanProgram): KontStack = initProg match
    case CleanProgram(decls, stmts, expr) =>
      val progClosure = (ProgFrame(decls, stmts, expr), Env())
      val initStack = List(progClosure)
      new StackifiedList(initStack)

  private class StackifiedList(underlying : List[Closure]) extends KontStack {
    def push(clo : Closure) : KontStack = new StackifiedList(clo :: underlying)
    def pop : KontStack = new StackifiedList(underlying.tail)
    def updateTopProgFrame(newProgFrame : ProgFrame) : KontStack = 
      val (_, oldEnv) = underlying.head
      val newClosure = (newProgFrame, oldEnv)
      new StackifiedList(newClosure :: underlying.tail)
  
    def isEmpty : Boolean = underlying.isEmpty
    def top : Closure = underlying.head
    def topProgFrame : ProgFrame = { val (pf, _) = underlying.head; pf }
    def topEnv : Env =  { val (_, env) = underlying.head; env }
  }
