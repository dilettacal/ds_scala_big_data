import exercise2.Problem5
import org.scalatest.FunSuite

class Problem5Test extends FunSuite {

  def testFunction(f: Int => Int): Unit = {
    //given as a correct example in the problem definition
    // the === operator is part of scala test and gives
    // a more sophisticated output than == if the assertion fails
    assert(f(10) === 2520)
  }

  test("bruteForce") {
    testFunction(Problem5.bruteForceImperativ)
  }
  test("bruteForceRecursion") {
    testFunction(Problem5.bruteForceRecursion)
  }

  test("bruteForceFunctional") {
    testFunction(Problem5.bruteForceFunctional)
  }
}
