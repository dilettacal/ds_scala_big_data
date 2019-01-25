object Problems99 extends App {

  case class MyBool(x: Boolean) {
    def and(that: MyBool): MyBool = if (x) that else this
    def or(that: MyBool): MyBool = if (x) this else that
    def negate: MyBool = MyBool(!x)
  }

  //001 Find the last element in a list
  def last(list: List[Int]): Int = list match {
    //list.last - builtin
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  //002 find the last but one element of a list
  //penultim

  def lastButOne(list: List[Int]): Int = list match {
    case h :: Nil => throw new NoSuchElementException
    case h :: _ :: Nil => h
    case _ :: tail => lastButOne(tail)
  }

  //Alternatives:
  def lastNthBuiltin[A](n: Int, ls: List[A]): A = {
    if (n <= 0) throw new IllegalArgumentException
    if (ls.length < n) throw new NoSuchElementException
    ls.takeRight(n).head
  }

  // Here's one approach to a non-builtin solution.
  def lastNthRecursive[A](n: Int, ls: List[A]): A = {
    def lastNthR(count: Int, resultList: List[A], curList: List[A]): A =
      curList match {
        case Nil if count > 0 => throw new NoSuchElementException
        case Nil => resultList.head
        case _ :: tail =>
          lastNthR(count - 1,
            if (count > 0) resultList else resultList.tail,
            tail)
      }
    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n, ls, ls)
  }

  println(lastButOne(List(1, 1, 2, 3, 5, 8))) //5

  //003 find the kth element of a list
  def nth(i: Int, list: List[Int]): Int = {
    if (i >= 0) list(i)
    else throw new NoSuchElementException
  }

  def nthRecursive(n: Int, ls: List[Int]): Int = (n, ls) match {
    case (0, h :: _) => h
    case (n, _ :: tail) => nthRecursive(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }

  //004 find the number of elements in a list
  def countElem(l: List[Int]): Int = l match {
    //l.length :-)
    case Nil => 0
    case _ :: tail => 1 + countElem(tail)
  }

  def countElemTailRec(l: List[Int]): Int = {
    //helper
    def lengthR(result: Int, curList: List[Int]): Int = curList match {
      case Nil => result
      case _ :: tail => lengthR(result + 1, tail)
    }
    lengthR(0, l)
  }

  def countElemHOF(l: List[Int]): Int = {
    l.foldLeft(0) { (c, _) => c + 1 }
  }

  println(countElem(List(1, 2, 45, 6, 2, 6, 16)))
  println(countElemTailRec(List(1, 2, 45, 6, 2, 6, 16)))
  println(countElemHOF(List(1, 2, 45, 6, 2, 6, 16)))

  //005 reverse a list
  def revers(l: List[Int]): List[Int] = l match {
    //l.reverse :-)
    case Nil => Nil
    case h :: tail => revers(tail) ::: List(h) //::: flatten
  }

  println(revers(List(1, 2, 45, 6, 2, 6, 16)))

  // Tail recursive.
  def reverseTailRecursive[A](ls: List[A]): List[A] = {
    def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil => result
      case h :: tail => reverseR(h :: result, tail)
    }
    reverseR(Nil, ls)
  }
  // Pure functional
  def reverseFunctional[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()) { (r, h) => h :: r }

  //006 is a list palindrome?
  def isPalindrome(l: List[Int]): Boolean = {
    l == revers(l)
  }

  println(isPalindrome(List(1, 2, 3, 2, 1)))

  //007 flatten a nested list structure
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

  //008 Eliminate consecutive duplicates of list elems
  def compressRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }

  // Tail recursive.
  def compressTailRecursive[A](ls: List[A]): List[A] = {
    def compressR(result: List[A], curList: List[A]): List[A] = curList match {
      case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
      case Nil => result.reverse
    }
    compressR(Nil, ls)
  }
  // Functional.
  def compressFunctional[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]()) { (h, r) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
    }

  //009 Pack consecutive duplicates of list into sublists
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span {
        _ == ls.head
      }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  //List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

  //010 Run-length-Encoding
  def encode[A](ls: List[A]): List[(Int, A)] =
    pack(ls) map { e => (e.length, e.head) }

  println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  //List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

  //011 - only duplicates are encoded
  def encodeModified[A](ls: List[A]): List[Any] =
    encode(ls) map { t => if (t._1 == 1) t._2 else t }


  //directly without any other methods like pack
  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span {
        _ == ls.head
      }
      (packed.length, packed.head) :: encodeDirect(next)
    }

  println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  //List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

  //012 decode
  //def decode[A](ls: List[(Int, A)]): List[A] =
  //ls flatMap { e => List.make(e._1, e._2) }

  //014 duplicate elements of a list
  def duplicate[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e) }

  println(duplicate(List('a, 'b, 'c, 'c, 'd)))
  //     res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  def dupe[T](xs: List[T]): List[T] =
    xs.foldRight(List[T]()) { (elem, acc) => elem :: elem :: acc }

  println(dupe(List('a, 'b, 'c, 'c, 'd)))

  //015 duplicate given a number of time
  def repeatElementsInList[T](list: List[T], times: Int): List[T] = {
    list.flatMap(x =>
      List.fill(times)(x)
    )
  }
  println(repeatElementsInList(List("a",1,"b"),3))
  //List(a, a, a, 1, 1, 1, b, b, b)

  val dup = List(1,1,1,2,3,4,5,5,6,100,101,101,102)
  println(dup.groupBy(identity).collect { case (x, List(_,_,_*)) => x })
  //List(101, 5, 1)

  //016 Drop every nth elem in a list
  def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
      case (_, Nil)       => Nil
      case (1, _ :: tail) => dropR(n, tail)
      case (_, h :: tail) => h :: dropR(c - 1, tail)
    }
    dropR(n, ls)
  }

  // Tail recursive.
  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A], result: List[A]): List[A] = (c, curList) match {
      case (_, Nil)       => result.reverse
      case (1, _ :: tail) => dropR(n, tail, result)
      case (_, h :: tail) => dropR(c - 1, tail, h :: result)
    }
    dropR(n, ls, Nil)
  }

  // Functional.
  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

  println(dropRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  //List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

  //017 split list into 2 parts
  def splitBuiltin[A](n: Int, ls: List[A]): (List[A], List[A]) = ls.splitAt(n)
  // Simple recursion.
  def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = (n, ls) match {
    case (_, Nil)       => (Nil, Nil)
    case (0, list)      => (Nil, list)
    case (n, h :: tail) => {
      val (pre, post) = splitRecursive(n - 1, tail)
      (h :: pre, post)
    }
  }
  // Tail recursive.
  def splitTailRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    def splitR(curN: Int, curL: List[A], pre: List[A]): (List[A], List[A]) =
      (curN, curL) match {
        case (_, Nil)       => (pre.reverse, Nil)
        case (0, list)      => (pre.reverse, list)
        case (n, h :: tail) => splitR(n - 1, tail, h :: pre)
      }
    splitR(n, ls, Nil)
  }

  // Functional (barely not "builtin").
  def splitFunctional[A](n: Int, ls: List[A]): (List[A], List[A]) =
    (ls.take(n), ls.drop(n))

  println(splitFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  //(List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

  //018 extract slice from list
  // Simple recursive.
  def sliceRecursive[A](start: Int, end: Int, ls: List[A]): List[A] =
    (start, end, ls) match {
      case (_, _, Nil)                 => Nil
      case (_, e, _)         if e <= 0 => Nil
      case (s, e, h :: tail) if s <= 0 => h :: sliceRecursive(0, e - 1, tail)
      case (s, e, h :: tail)           => sliceRecursive(s - 1, e - 1, tail)
    }

  // Tail recursive, using pattern matching.
  def sliceTailRecursive[A](start: Int, end: Int, ls: List[A]): List[A] = {
    def sliceR(count: Int, curList: List[A], result: List[A]): List[A] =
      (count, curList) match {
        case (_, Nil)                     => result.reverse
        case (c, h :: tail) if end <= c   => result.reverse
        case (c, h :: tail) if start <= c => sliceR(c + 1, tail, h :: result)
        case (c, _ :: tail)               => sliceR(c + 1, tail, result)
      }
    sliceR(0, ls, Nil)
  }
  // Functional.
  def sliceFunctional[A](s: Int, e: Int, ls: List[A]): List[A] =
    ls drop s take (e - (s max 0))

  println(sliceFunctional(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  //List[Symbol] = List('d, 'e, 'f, 'g)

  //019 rotate list N places to the left
  def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotate(nBounded + ls.length, ls)
    else (ls drop nBounded) ::: (ls take nBounded)
  }

  //020 remove kth element returning removed elem and list as tuple
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException
  }
  println(removeAt(1, List('a, 'b, 'c, 'd)))
  //(List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

  //021 insert element at a given position into a list
  def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = ls.splitAt(n) match {
    case (pre, post) => pre ::: e :: post
  }

  println(insertAt('new, 1, List('a, 'b, 'c, 'd)))
  //List[Symbol] = List('a, 'new, 'b, 'c, 'd)

  //022 Create a list of int within a range
  // Builtin.
  def rangeBuiltin(start: Int, end: Int): List[Int] = List.range(start, end + 1)

  // Recursive.
  def rangeRecursive(start: Int, end: Int): List[Int] =
    if (end < start) Nil
    else start :: rangeRecursive(start + 1, end)

  // Tail recursive.
  def rangeTailRecursive(start: Int, end: Int): List[Int] = {
    def rangeR(end: Int, result: List[Int]): List[Int] = {
      if (end < start) result
      else rangeR(end - 1, end :: result)
    }
    rangeR(end, Nil)
  }

  // The classic functional approach would be to use `unfoldr`, which Scala
  // doesn't have.  So we'll write one and then use it.
  def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] =
    f(s) match {
      case None         => Nil
      case Some((r, n)) => r :: unfoldRight(n)(f)
    }
  def rangeFunctional(start: Int, end: Int): List[Int] =
    unfoldRight(start) { n =>
      if (n > end) None
      else Some((n, n + 1))
    }

  println(rangeTailRecursive(4, 9))
  //List[Int] = List(4, 5, 6, 7, 8, 9)

  //023 -Extract a given number of randomly selected elements from a list.
  def randomSelect1[A](n: Int, ls: List[A]): List[A] =
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
      e :: randomSelect1(n - 1, rest)
    }

  //024 Lotto - draw n different random numbers
  def lotto(count: Int, max: Int): List[Int] =
    randomSelect1(count, List.range(1, max + 1))

  //025 - Generate a random permutation of the elements of a list.
  /* def randomPermute[A](ls: List[A]): List[A] = {
    val rand = new util.Random
    val a = ls.toArray
    for (i <- a.length - 1 to 1 by -1) {
      val i1 = rand.nextInt(i + 1)
      val t = a(i)
      a.update(i, a(i1))
      a.update(i1, t)
    }
    a.toList
  } */

///ARITHMETIC
  //031 Determine whether a given integer number is prime.
  def isPrime(num: Int): Boolean = (2 to num) forall (x => num % x != 0)

  //032 Determine the greatest common divisor of two positive integer numbers.
  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  //033 Determine whether two positive integer numbers are coprime.
  def isCoprimeTo(n: Int): Boolean = gcd(0, n) == 1 //0 == start

  //034 Calculate Euler's totient function phi(m).
  // def totient: Int = (1 to start) filter { start.isCoprimeTo(_) } length

  //035 Determine the prime factors of a given positive integer.
  /* def primeFactors: List[Int] = {
     def primeFactorsR(n: Int, ps: Stream[Int]): List[Int] =
       if (isPrime(n)) List(n)
       else if (n % ps.head == 0) ps.head :: primeFactorsR(n / ps.head, ps)
       else primeFactorsR(n, ps.tail)
     primeFactorsR(start, primes)
   }
 */


}



