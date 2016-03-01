
// A comment !
/* Another comment */
/** A documentation comment **/

/*
 * The object keyword creates a new singleton type, which means that MyModule is the only value (or 'inhabitant') of that type.
 * The Chapter2 object has three methods: abs, formatAbs, and main
 * For methods : "def" left-hand side or signature "=" right-hand side or definition
 */

object MyModule {
  
  def abs(n: Int): Int = //the colon is pronounced "has type", here "has type Int"
    if (n < 0) -n
    else n
    
  // This method is declared private, which means that it cannot be called from any code outside of the MyModule object
  // No return type : an optional type annotation indicates the type of the result
  // A function that takes another function as an argument is called a higher-order function
  private def formatResult(name:String, n: Int, f:Int => Int) = { 
      //A val is an immutable variable
      val msg = "The %s value of %d is %d"
      msg.format(name, n, f(n))
    }
  
  def factorial(n:Int): Int = {
    // The way we write loops in Scala is with a recursive function, by convention often called go
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
      
    go(n,1)
  }
  
  def fib(n: Int): Int = {
    def go(n: Int): Int =
      if (n <= 1) n
      else go(n-1) + go(n-2)
    
    go(n)
  }
  
  // Polymorphic function
  def binarySearch[A](as: Array[A], key:A, greaterThan: (A,A) => Boolean): Int = {
    //If all recursive calls made by a function are in tail position, Scala compiles the recursion to iterative loops that do not consume call stack
    //frames for each iteration. If we are expecting this to occur for a recursive function we write, we can tell the Scala compiler about this
    //assumption using an annotation:
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid -1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = greaterThan(a, key)
        if (!greater && !greaterThan(key,a)) mid2 // Then it's equal
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 +1, mid2, high)
      }
    }
    go(0, 0, as.length -1)
  }
  
  // Polymorphic function
  def isSorted[A](as: Array[A], greaterThan: (A,A) => Boolean): Boolean = {
    //If all recursive calls made by a function are in tail position, Scala compiles the recursion to iterative loops that do not consume call stack
    //frames for each iteration. If we are expecting this to occur for a recursive function we write, we can tell the Scala compiler about this
    //assumption using an annotation:
    @annotation.tailrec
    def go(n: Int, max: Int): Boolean = {
      if (n == max) true
      else if (greaterThan(as(n+1),as(n))) go(n + 1, max)
      else false
    }
    go(0, as.length -1)
  }
  
  //a higher-order function for doing what is called partial application: takes a value and a function of two
  //arguments, and returns a function of one argument as its result
  //see example in main
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    f(a,_)
    //(b: B) => f(a, b)
  }
  
  //see example in main
  def curry[A,B,C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
   }
  
    
  //The return type of Unit indicates that this method does not return a meaningful value
  def main(args: Array[String]): Unit = {
    
    println(formatResult("absolute value",-42, abs))
    println(formatResult("factorial",7, factorial))
    println(formatResult("Fibonacci number",6, fib))
    
    //Scala provides a syntax for declaring these nameless or anonymous functions, often called function literals, lambda functions, lambda expressions, or just lambdas 
    println(formatResult("lamba1", 7, (x: Int) => x + 1))
    println(formatResult("lamba2", 7, (x) => x + 1))
    println(formatResult("lamba3", 7, x => x + 1))
    println(formatResult("lamba4", 7, _ + 1 )) //sometimes called underscore syntax for a function literal
    println(formatResult("lamba5", 7, x => {val r = x + 1; r}))
    
    val as1 = Array(1,2,3,10)
    println("as1 is sorted : " + isSorted(as1,(x: Int,y: Int) => x > y).toString())
    val as2 = Array(1.1,7.2,6.9)
    println("as2 is sorted : " + isSorted(as2,(x: Double,y: Double) => x > y).toString())

    val multiplyByTwo = partial1(2, (a:Int, b:Int) => a * b)
    println("multiplyByTwo of %d is %d".format(5,multiplyByTwo(5)))
    
    val multiplybyX = curry((a:Int, b:Int) => a * b)
    val multiplyby3 = multiplybyX(3)
    println("multiplyBy3 of %d is %d".format(5,multiplyby3(5)))
  }
  
}
