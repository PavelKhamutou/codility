package demo

/*
This is a demo task. You can read about this task and its solutions in this blog post.
A zero-indexed array A consisting of N integers is given. An equilibrium index of this array is any integer P such that 0 ≤ P < N and the sum of elements of lower indices is equal to the sum of elements of higher indices, i.e.
    A[0] + A[1] + ... + A[P−1] = A[P+1] + ... + A[N−2] + A[N−1].
Sum of zero elements is assumed to be equal to 0. This can happen if P = 0 or if P = N−1.
For example, consider the following array A consisting of N = 8 elements:
  A[0] = -1
  A[1] =  3
  A[2] = -4
  A[3] =  5
  A[4] =  1
  A[5] = -6
  A[6] =  2
  A[7] =  1
P = 1 is an equilibrium index of this array, because:
        A[0] = −1 = A[2] + A[3] + A[4] + A[5] + A[6] + A[7]
P = 3 is an equilibrium index of this array, because:
        A[0] + A[1] + A[2] = −2 = A[4] + A[5] + A[6] + A[7]
P = 7 is also an equilibrium index, because:
        A[0] + A[1] + A[2] + A[3] + A[4] + A[5] + A[6] = 0
and there are no elements with indices greater than 7.
P = 8 is not an equilibrium index, because it does not fulfill the condition 0 ≤ P < N.
Write a function:
    int solution(int A[], int N);
that, given a zero-indexed array A consisting of N integers, returns any of its equilibrium indices. The function should return −1 if no equilibrium index exists.
For example, given array A shown above, the function may return 1, 3 or 7, as explained above.
Assume that:
        N is an integer within the range [0..100,000];
        each element of array A is an integer within the range [−2,147,483,648..2,147,483,647].
Complexity:
        expected worst-case time complexity is O(N);
        expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
Elements of input arrays can be modified.
 */

import scala.collection.mutable.ArrayBuffer

object Equi extends App {
  val A = Array[Int](-1, 3, -4, 5, 1, -6, 2, 1)
  //val A = Array[Int](3, 2, -5, 1)

  val e = new Equi()

  var startTime = System.currentTimeMillis()
  e.equi(A, 8)
  var finishTime = System.currentTimeMillis()
  println("Time1: " + (finishTime - startTime))

  startTime = System.currentTimeMillis()
  e.equi1line(A, 8)
  finishTime = System.currentTimeMillis()
  println("Time2: " + (finishTime - startTime))

  startTime = System.currentTimeMillis()
  e.equiOn(A, 8)
  finishTime = System.currentTimeMillis()
  println("Time3: " + (finishTime - startTime))
}

class Equi {
  // 0<= k and sum(A[0..(k-1)])=sum(A[(k+1)..(n-1)])
  def equi(A: Array[Int], n: Int): Int = {

    val ab = ArrayBuffer[Int]()
    var sum_prefix = 0
    var sum_sufix = 0

    for(i <- 0 until n){
      sum_prefix = 0
      for(j <- 0 until i) {
        sum_prefix += A(j)
      }
      sum_sufix = 0
      for(j <- (i+1) until n) {
        sum_sufix += A(j)
      }
      if(sum_prefix == sum_sufix) ab += i
    }
    println(ab.mkString(" "))
    -1
  }

  def equi1line(A: Array[Int], n: Int): Int = {
    val ab = ArrayBuffer[Int]()
    for(i <- 0 until n) {
      if (A.slice(0, i).sum == A.slice(i + 1, n).sum) ab += i
    }
    println(ab.mkString(" "))
    -1
  }

  def equiOn(A: Array[Int], n: Int): Int = {
    if(n == 0) return -1

    val ab = ArrayBuffer[Int]()
    var sum: Int = 0

    for(i <- 0 until n) sum += A(i)

    var sum_left = 0
    for(i <- 0 until n){
      val sum_right = sum - sum_left - A(i)
      if(sum_right == sum_left) ab += i
      sum_left += A(i)
    }

    println(ab.mkString(" "))
    -1

  }
}