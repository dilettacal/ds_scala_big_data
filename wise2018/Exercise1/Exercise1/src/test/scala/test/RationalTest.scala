package test

import exercise1.Rational
import org.scalatest.FunSuite

//see http://www.scalatest.org/at_a_glance/FunSuite
class RationalTest extends FunSuite {
  
  test("Rational Inititalisation 1") {
    val x = new Rational(1,2)
    assert(x.value === 0.5)
  }
  
  test("Rational Inititalisation 2") {
    val x = new Rational(1,2)
    assertResult("1/2"){x.toString}
  }

  test("Test requirement (denominator!=0)"){
      intercept [IllegalArgumentException] {
        new Rational(1,0)}
  }

  test("Test add"){
    //TODO
    fail()
  }
  
  test("Test sub"){
    //TODO
    fail()
  }

  //TODO tests for other methods similar to the above..
}
