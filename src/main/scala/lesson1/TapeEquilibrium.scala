package lesson1

/*
A non-empty zero-indexed array A consisting of N integers is given. Array A represents numbers on a tape.
Any integer P, such that 0 < P < N, splits this tape into two non-empty parts: A[0], A[1], ..., A[P − 1] and A[P], A[P + 1], ..., A[N − 1].
The difference between the two parts is the value of: |(A[0] + A[1] + ... + A[P − 1]) − (A[P] + A[P + 1] + ... + A[N − 1])|
In other words, it is the absolute difference between the sum of the first part and the sum of the second part.
For example, consider array A such that:
  A[0] = 3
  A[1] = 1
  A[2] = 2
  A[3] = 4
  A[4] = 3
We can split this tape in four places:
        P = 1, difference = |3 − 10| = 7
        P = 2, difference = |4 − 9| = 5
        P = 3, difference = |6 − 7| = 1
        P = 4, difference = |10 − 3| = 7
Write a function:
    object Solution { def solution(A: Array[Int]): Int }
that, given a non-empty zero-indexed array A of N integers, returns the minimal difference that can be achieved.
For example, given:

  A[0] = 3
  A[1] = 1
  A[2] = 2
  A[3] = 4
  A[4] = 3
the function should return 1, as explained above.
Assume that:
        N is an integer within the range [2..100,000];
        each element of array A is an integer within the range [−1,000..1,000].
Complexity:
        expected worst-case time complexity is O(N);
        expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).

 */



object TapeEquilibrium extends App{
  def solution(A: Array[Int]): Int = {
    //O(N^2)
    if(A.isEmpty) return 0
    if(A.length == 1) return A(0)

    var lastIndex = A.length - 1
    var sum = A.splitAt(lastIndex)

    val r = for(i <- 0 until (A.length - 1)) yield {
      val sum = A.splitAt(lastIndex)
      lastIndex -= 1
      Math.abs(sum._1.sum - sum._2.sum)
    }
    r.min
  }
  def solutionRecursion(A: Array[Int]): Int = {
    //O(N^2)
    def recFun(head: Int, tail: Array[Int], currVal: Int): Int = {
      val r = Math.abs(head - tail.sum)
      if(tail.length == 1) {
        if(currVal > r) r
        else currVal
      }
      else if(currVal > r) {
        recFun(head + tail.head, tail.tail, r)
      }
      else recFun(head + tail.head, tail.tail, currVal)
    }
    recFun(A.head, A.tail, Math.abs(A.head - A.tail.sum))

  }

  def solutionWithForv1(A: Array[Int]): Int = {
    //O(N)
    if(A.isEmpty) return 0
    if(A.length == 1) return A(0)

    val aReversed = A.reverse
    var fsum = A.init.sum
    var lsum = aReversed.head
    var d1 = Math.abs(fsum - lsum)

    for(i <- 1 until (A.length - 1)) {
      fsum = fsum - aReversed(i)
      lsum = lsum + aReversed(i)
      val d2 = Math.abs(fsum - lsum)
      if(d1 > d2) d1 = d2
    }
    d1
  }

  def solutionWithForv2(A: Array[Int]): Int = {
    //O(N) a bit faster then solutionWithForv1 2.332s over 2.338s
    if(A.isEmpty) return 0
    if(A.length == 1) return A(0)

    val lastIndex = A.length - 1
    var fsum = A.init.sum
    var lsum = A(lastIndex)
    var d1 = Math.abs(fsum - lsum)

    for(i <- 1 until (A.length - 1)) {
      fsum = fsum - A(lastIndex - i)
      lsum = lsum + A(lastIndex - i)
      val d2 = Math.abs(fsum - lsum)
      if(d1 > d2) d1 = d2
    }
    d1
  }

  val chechArray = Array(3, 1, 2, 4, 3)
  //println(solution(chechArray))
  //println(solutionRecursion(chechArray))
  println(solutionWithForv2(chechArray))

}
