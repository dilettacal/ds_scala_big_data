import exercise2.Problem7
import org.scalatest.FunSuite

class Problem7Test extends FunSuite {

  def testFunction(f: Int => Int): Unit = {
    //given as a correct example in the problem definition
    // the === operator is part of scala test and gives
    // a more sophisticated output than == if the assertion fails
    assert(f(6) === 13)
  }

  test("bruteForce") {
    testFunction(Problem7.bruteForceImperativ)
  }

  test("divisionWithPrimes") {
    testFunction(Problem7.divisionWithPrimesImperativ)
  }
}
