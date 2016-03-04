package Chapter3

object MyModule {
  
 def main(args: Array[String]): Unit = {
 
    val example1 = SSL(1, SSL(2, SSL(3,Vide)))
    val example2 = Liste(1,2,3,7,8,9)
    val total1 = Liste.sum(example1) 
    println(total1)
      
    println(Liste(1,2,3) match { case _ => 42 })
    println(Liste(1,2,3) match { 
      case Vide => 42
      case SSL(h,t) => h 
    })
    println(Liste(1,2,3) match { 
      case Vide => 42  
      case SSL(_,t) => t 
      case _ => 101
     })
    
    val x = Liste(1,2,3,4,5) match {
      case SSL(x, SSL(2, SSL(4, _))) => x
      case Vide => 42
      case SSL(x, SSL(y, SSL(3, SSL(4, _)))) => x + y // premier match
      case SSL(h, t) => h + Liste.sum(t) // second match
      case _ => 101 // troisieme match
    }
    
    println("Exercice 1 : %d".format(x))
    
    val example2tailed = Liste.tail(example2)
    println("Exercice 2 : %s".format(example2tailed.toString()))
    
    val example2dropped = Liste.drop(example2,4)
    println("Exercice 3 : %s".format(example2dropped.toString()))

    val example2droppedWhile = Liste.dropWhile(example2)((x: Int) => x < 8)
    println("Exercice 4 : %s".format(example2droppedWhile.toString()))
    
    val example2setHead = Liste.setHead(example2,99)
    println("Exercice 5 : %s".format(example2setHead.toString()))

    val example2init = Liste.init(example2)
    println("Exercice 6 : %s".format(example2init.toString()))
    
   val total12 = Liste.sum2(example1) 
    println(total12)
  } 
 
}