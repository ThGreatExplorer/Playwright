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
  def isFinal : Boolean =
    control match { 
      case Control.Err(_) => kont.isEmpty
      case Control.Value(_) => kont.isEmpty
      case _ => false
    }

  def lookupVar(x : String) : CESKValue = 
    val loc = env.getLoc(x) 
    store.getVal(loc) 

  override def toString() : String = 
      "\nControl: "     + control 
    + "\nEnvironment: " + env
    + "\nStore: "       + store
    + "\nKont: "        + kont

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

object CESKConst:
  val TRUTHY     : NumVal = 0.0
  val FALSY      : NumVal = 1.0
  val BLOCKFLAG  : Unit   = ()
  val DUMMYFLAG  : Unit   = ()
  val DUMMYFRAME : ProgFrame = ProgFrame(Nil, Nil, DUMMYFLAG)
/******************************************************************************
  Below we define interfaces and type aliases for CESK registers. 
  
  Most of these are thin wrappers around existing Scala data sctructures, but 
  this implementation allows us to encapsulate our implementation details and 
  swap out the underlying structure if desired.

 *****************************************************************************/

// Our CESK Machine understands three types of runtime values: numbers, objects
// and proxies (i.e. object + shape)
type CESKValue = NumVal | ObjectVal | ProxyVal

// Locations corresponding to Values in Store. We want to have a (practically)
// infinite set of possible locations such that we can always produce a fresh
// location. Iterator built on Integers fits our needs nicely. 
type Loc = Integer

// Control Register is an enum
enum Control:
  case Expr(e : CleanExpr)
  case Value(n : CESKValue)
  case Err(e : RuntimeError)
  case Search

// Env Register is a Map from Strings to Locations with the following interface
trait Env:
  def updatedEnv(x : String, loc : Loc) : Env
  def getLoc(x : String) : Loc
  override def toString(): String

object Env:

  // Constructor that returns instances with the Env trait without actually
  // exposing internal imlementation of the underlying private class
  def apply(entries: (String, Loc)*): Env = 
    new MapEnv(Map(entries*))

  // Actual class we instantiate that is hidden from users
  private class MapEnv(underlying : Map[String, Loc]) extends Env {

    override def toString(): String = underlying.toString()
    
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
  def insertValAtNewLoc(n: CESKValue) : (Store, Loc)
  def updatedStore(loc: Loc, n: CESKValue) : Store
  def getVal(loc : Loc) : CESKValue
  override def toString(): String

object Store:

  def apply(entries: (Loc, CESKValue)*): Store = 
    new MapStore(Map(entries*))

  // Private iterator that will produce fresh locations 
  // Is only accessed from within the private MapStore class
  private val locactionGenerator = Iterator.from(0)
  private def freshLoc() : Loc = locactionGenerator.next()

  private class MapStore(underlying : Map[Loc, CESKValue]) extends Store {
    override def toString(): String = underlying.toString()

    def insertValAtNewLoc(n : CESKValue) : (Store, Loc) =
      val loc = freshLoc();
      val newStore = new MapStore(underlying.updated(loc, n))
      (newStore, loc)

    def updatedStore(loc : Loc, n : CESKValue) : Store =
      new MapStore(underlying.updated(loc, n))

    def getVal(loc : Loc) : CESKValue =
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
final case class ProgFrame(
  decls: List[CleanDecl], 
  stmts: List[CleanStmt | CleanStmtBlock], 
  expr: CleanExpr | Unit
)

// Starting with assignment 12, our stack also contains range types to ensure
// the value returned from a method call conforms to an expected type
type RangeT = CleanType

// Kont Register is a Stack of Closures with the following interface
trait KontStack:
  override def toString(): String
  def length : Int
  def push(clo : Closure) : KontStack
  def pop : KontStack
  def updateTopProgFrame(newProgFrame : ProgFrame) : KontStack
  def isEmpty : Boolean 
  def top : ProgFrame | RangeT
  def topEnv : Env 

object KontStack:

  def apply(entries: Closure*) : KontStack = new StackifiedList(List(entries*))

  // Special constructor called from load 
  def constructWithTL(initProg : CleanProgram): KontStack = initProg match
    case Program[Clean](_, ProgBlock(decls, stmts, expr)) =>
      val progClosure  = (ProgFrame(decls, stmts, expr), Env())
      val initStack = List(progClosure) 
      new StackifiedList(initStack)

  private class StackifiedList(underlying : List[Closure | RangeT]) extends KontStack {
    override def toString(): String = underlying.toString()
    def push(clo : Closure) : KontStack = new StackifiedList(clo :: underlying)
    def pop : KontStack = new StackifiedList(underlying.tail)
    def updateTopProgFrame(newProgFrame : ProgFrame) : KontStack = 
      underlying.head match
        case clo : Closure => 
          val (_, oldEnv) = clo
          val newClosure = (newProgFrame, oldEnv)
          new StackifiedList(newClosure :: underlying.tail)

        case rT : RangeT => 
          throw new UnreachablePatternMatch(
            "Should never happen: attemped to update topProgFrame, but got range type"
          )       
  
    def isEmpty : Boolean = underlying.isEmpty
    def length : Int = underlying.length

    def top : ProgFrame | RangeT = 
      underlying.headOption match
        case Some(clo : Closure) => val (pf, _) = clo; pf 

        case Some(rT : RangeT) => rT

        // Boostrapping transition() at Top Level return expression evaluation 
        case None => CESKConst.DUMMYFRAME 

    def topEnv : Env = 
      underlying.headOption match
        case Some(clo : Closure) => val (_, env) = clo; env 

        case None => 
          throw new UnreachablePatternMatch(
            "Should never happen: attemped to get topEnv from empty KontStack"
          ) 

        case Some(rT : RangeT) => 
          throw new UnreachablePatternMatch(
            "Should never happen: attemped to get topEnv, but got range type"
          )   
  }
