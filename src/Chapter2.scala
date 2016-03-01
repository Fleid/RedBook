
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
  
  //The return type of Unit indicates that this method does not return a meaningful value
  def main(args: Array[String]): Unit =
    println(formatResult("absolute value",-42, abs))
    println(formatResult("factorial",7, factorial))
    println(formatResult("Fibonacci number",6, fib))
    
    //Scala provides a syntax for declaring these nameless or anonymous functions, often called function literals, lambda functions, lambda expressions, or just lambdas
    
    println(formatResult("lamba1", 7, (x: Int) => x + 1))
    println(formatResult("lamba2", 7, (x) => x + 1))
    println(formatResult("lamba3", 7, x => x + 1))
    println(formatResult("lamba4", 7, _ + 1 ))
    println(formatResult("lamba5", 7, x => {val r = x + 1; r}))
}
