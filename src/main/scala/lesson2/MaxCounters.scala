package lesson2
/*
You are given N counters, initially set to 0, and you have two possible operations on them:
        increase(X) − counter X is increased by 1,
        max counter − all counters are set to the maximum value of any counter.
A non-empty zero-indexed array A of M integers is given. This array represents consecutive operations:
        if A[K] = X, such that 1 ≤ X ≤ N, then operation K is increase(X),
        if A[K] = N + 1 then operation K is max counter.
For example, given integer N = 5 and array A such that:
    A[0] = 3
    A[1] = 4
    A[2] = 4
    A[3] = 6
    A[4] = 1
    A[5] = 4
    A[6] = 4
the values of the counters after each consecutive operation will be:
    (0, 0, 1, 0, 0)
    (0, 0, 1, 1, 0)
    (0, 0, 1, 2, 0)
    (2, 2, 2, 2, 2)
    (3, 2, 2, 2, 2)
    (3, 2, 2, 3, 2)
    (3, 2, 2, 4, 2)
The goal is to calculate the value of every counter after all operations.
Write a function:
    object Solution { def solution(N: Int, A: Array[Int]): Array[Int] }
that, given an integer N and a non-empty zero-indexed array A consisting of M integers, returns a sequence of integers representing the values of the counters.
The sequence should be returned as:
        a structure Results (in C), or
        a vector of integers (in C++), or
        a record Results (in Pascal), or
        an array of integers (in any other programming language).
For example, given:
    A[0] = 3
    A[1] = 4
    A[2] = 4
    A[3] = 6
    A[4] = 1
    A[5] = 4
    A[6] = 4
the function should return [3, 2, 2, 4, 2], as explained above.
Assume that:
        N and M are integers within the range [1..100,000];
        each element of array A is an integer within the range [1..N + 1].
Complexity:
        expected worst-case time complexity is O(N+M);
        expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
Elements of input arrays can be modified.
 */
object MaxCounters extends App {
  def solution(N: Int, A: Array[Int]): Array[Int] = {
    var lastMax = 0
    val counters: Array[Int] = new Array[Int](N)
    for(i <- A) {
      println("enter: " + counters.mkString(" "))
      if(i >= 1 && i <= N) counters(i - 1) += 1
      if(i == N + 1) {
        val max = counters.max
        if(max != lastMax) {
          for(j <- 0 until counters.length) {
            counters(j) = max
            lastMax = max
          }
        }
      }
    }
    counters
  }

  def solution2(N: Int, A: Array[Int]): Array[Int] = {
    //O(N + M)
    val n1 = N + 1
    var counters: Array[Int] = new Array[Int](N)
    var lastValue = 0
    var max = 0
    for{
      i <- A
      if !(i == n1 && lastValue == n1)
    } {
      if(i >= 1 && i <= N) {
        counters(i - 1) += 1
        if(counters(i - 1) > max) max = counters(i - 1)
      }
      if(i == n1) counters = Array.fill(N)(max)
      lastValue = i
    }
    counters
  }


//  if A[K] = X, such that 1 ≤ X ≤ N, then operation K is increase(X),
//  if A[K] = N + 1 then operation K is max counter.


  val a = Array(3, 4, 4, 6, 1, 4, 4)
  val a1 = Array(1, 6, 1, 6, 6, 6, 6)
  val a2 = Array(1, 6, 5, 5, 5, 5, 5)

  println("resilt: " + solution2(5, a2).mkString(" "))

}
