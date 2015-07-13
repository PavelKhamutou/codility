package lesson2
/*
Write a function:
    object Solution { def solution(A: Array[Int]): Int }
that, given a non-empty zero-indexed array A of N integers, returns the minimal positive integer (greater than 0) that does not occur in A.
For example, given:
  A[0] = 1
  A[1] = 3
  A[2] = 6
  A[3] = 4
  A[4] = 1
  A[5] = 2
the function should return 5.
Assume that:
        N is an integer within the range [1..100,000];
        each element of array A is an integer within the range [−2,147,483,648..2,147,483,647].
Complexity:
        expected worst-case time complexity is O(N);
        expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
Elements of input arrays can be modified.
 */
object MissingInteger extends App {
  def solution(A: Array[Int]): Int = {
    //O(N)
    val counter = new Array[Int](A.length)
    for(i <- A if i > 0) {
      if(i <= counter.length) counter(i - 1) += 1
    }
    for(i <- 0 until counter.length) {
      if(counter(i) == 0) return i + 1
    }
    counter.length + 1
  }

  val a = Array(-1, -3, -6, 4, 1, 2)
  val a2 = Array(1)
  println(solution(a))
  println(solution(a2))
}
