package lesson1

/*
A zero-indexed array A consisting of N different integers is given. The array contains integers in the range [1..(N + 1)], which means that exactly one element is missing.
Your goal is to find that missing element.
Write a function:
    object Solution { def solution(A: Array[Int]): Int }
that, given a zero-indexed array A, returns the value of the missing element.
For example, given array A such that:
  A[0] = 2
  A[1] = 3
  A[2] = 1
  A[3] = 5
the function should return 4, as it is the missing element.
Assume that:
        N is an integer within the range [0..100,000];
        the elements of A are all distinct;
        each element of array A is an integer within the range [1..(N + 1)].
Complexity:
        expected worst-case time complexity is O(N);
        expected worst-case space complexity is O(1), beyond input storage (not counting the storage required for input arguments).
Elements of input arrays can be modified
 */
object PermMissingElem extends App {
  def solution(A: Array[Int]): Int = {
    if(A.isEmpty) return 1
    val max = A.length + 1
    //val rightTotal = (A.length*(A.length+1))/2
    var rightTotal = 0
    for(i <- 1 to A.length) rightTotal += i
    val changedTotal = A.sum
    max - (changedTotal - rightTotal)
  }

  def solution2(A: Array[Int]): Int = {
    //O(N) or O(N * log(N))
    if(A.isEmpty) return 1
    var sum: Double = 0.5 * ( A.length + 1) * (A.length + 2)
    for(i <- A)  sum -=i
    sum.toInt
  }
  val checkArray = Array(2, 3, 1, 5)
  println(solution(checkArray)) // should return 4
  println(solution2(checkArray))
}
