object ForLoops extends App {

  for(i <- 1 to 10) {
    println("i is " + i);
  }
  println(1.to(10)) //Range 1 to 10

  var myArray : Array[String] = new Array[String](10);
  for(i <- 0 until myArray.length){
    myArray(i) = "value is: " + i;
  }
  // the for loop only executes its body if the string assigned value ends with the text "5"
  for(value : String <- myArray if value.endsWith("5")) {
    println(value);
  }
  for(value : String <- myArray
      if value.endsWith("5");
      if value.indexOf("value") != -1 ) {

    println(value);
  }

  //Nested for loops
  var myArray2 : Array[Array[String]] = new Array[Array[String]](10);
  for(i <- 0 until myArray2.length){
    myArray2(i) = new Array[String](10);
    for(j <- 0 until myArray2(i).length){
      myArray2(i)(j) = "value is: " + i + ", " + j;
    }
  }
  for(anArray : Array[String] <- myArray2;
      aString : String        <- anArray ) {
    println(aString);
  }


  //YIELD
  /*
  For each iteration of your for loop,
  yield generates a value which will be remembered.
  It's like the for loop has a buffer, and
  for each iteration of your for loop another
  item is added to that buffer
   */
  for (i <- 1 to 5) yield i
  for (i <- 1 to 5) yield i * 2

  val a = Array(1, 2, 3, 4, 5)
  for (e <- a) yield e //Array[Int] = Array(1, 2, 3, 4, 5)
  for (e <- a) yield e * 2 //Array(2, 4, 6, 8, 10)
  for (e <- a if e > 2) yield e //Array(3, 4, 5)

//  for(x <- c1; y <- c2; z <-c3) --> c1.foreach(x => c2.foreach(y => c3.foreach(z =>
 // for(x <- c1; y <- c2; z <- c3) yield -->  c1.flatMap(x => c2.flatMap(y => c3.map(z

}
