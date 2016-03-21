package Chapter3


// "sealed" : all the subclasses are definied in this file -> Companion object must be below
 // "trait" : abstract interface, used to define object types
 // +A : the + indicates that the type parameter, A is covariant
sealed trait Liste[+A]
/*
 * In the declaration , the in trait List[+A] + front of the type parameter A is a variance annotation which signals 
 * that A is a covariant or 'positive' parameter of List. This means that, for instance, List[Dog] is considered
 * a subtype of List[Animal], assuming Dog is a subtype of Animal
 */

//2 data constructors for Liste for its 2 states: empty or not
 // "case object" are objects, that are serializable, and have readable toString outputs
   // The case object Vide lets us write Vide to construct an empty List
case object Vide extends Liste[Nothing] //heritage - C'est l'object immutable liste vide dont on va avoir besoin fatalement
 // "case class" are classes, that have serialization, readable toString, pattern matching, equals, hashCode... 
   // The case class SSL lets us write SSL(1, Nil), SSL(1,SSL(2, Nil)), and so on for nonempty lists)
case class SSL[+A](head: A, tail: Liste[A]) extends Liste[A] // Object Singly-Linked List



// Companion object of Liste, vu qu'on est sealed, l'objet doit etre dans ce fichier
object Liste{
  def sum(ints: Liste[Int]): Int = ints match { //"match" : pattern matching, genre de switch
    case Vide => 0   // syntaxe du switch:  case ... => ...
    case SSL(x,xs) => x + sum(xs)
  }
  
  def product(ds: Liste[Double]): Double = ds match {
    case Vide => 1.0
    case SSL(0.0,_) => 0.0
    case SSL(x,xs) => x * product(xs)
  }
   
  def apply[A](as: A*): Liste[A] = {
    if (as.isEmpty) Vide
    else SSL(as.head, apply(as.tail: _*))
  }
  
//3.3 Functional data structures and data sharing
  //Ex 2
  def tail[A](l: Liste[A]): Liste[A] = l match {
    case Vide => Vide
    case SSL(x,xs) => xs
  }
  //Ex 3 
  def drop[A](l: Liste[A], i: Int): Liste[A] = l match {
    case Vide => Vide
    case SSL(x,xs) => {if (i<=0) l else drop(xs,i-1)}
  }
  
  //Ex 4
  //def dropWhile[A](l: Liste[A], f: A => Boolean): Liste[A] = l match { //en mode newbie
  def dropWhile[A](l: Liste[A])(f: A => Boolean): Liste[A] = l match { //en mode mieux pour l'inference
    case Vide => Vide
    case SSL(x,xs) => (if (f(x)) dropWhile(xs)(f) else xs) 
  }
  
  def setHead[A](l: Liste[A], a: A): Liste[A] = l match {
    case Vide => Liste(a)
    case SSL(x,xs) => SSL(a,xs)
  }
  
  //Ex 5
  def append[A](a1: Liste[A], a2: Liste[A]): Liste[A] = a1 match {
    case Vide => a2
    case SSL(h,t) => SSL(h, append(t, a2))
  }
  
  //Ex 6
  def init[A](l: Liste[A]): Liste[A] = l match {
    case Vide => Vide
    case SSL(_,Vide) => Vide
    case SSL(h,t) => SSL(h,init(t))
  }
  
//3.4 Recursion over lists and generalizing to higher-order functions
  
  
  //Again, placing f in its own argument group after l and z lets type inference determine the input types to f. See dropWhile
  def foldRight[A,B](l: Liste[A], z:B)(f: (A,B) => B): B = l match {
    case Vide => z
    case SSL(x,xs) => f(x,foldRight(xs,z)(f))
  }

  def sum2(l: Liste[Int])=
    foldRight(l,0)(_ + _)
    
  def product2(l: Liste[Double])=
    foldRight(l,1.0)(_ * _)
    
  //Ex 7 : No way for product2 to interrupt the loop since it's in foldRight, we'll have to add an interrupt switch to foldRight
  
  //Ex 9 : Compute the length of a list using foldRight
  def length[A](l: Liste[A]): Int = l match {
    case Vide => 0
    case SSL(_,xs) => 1 + length(xs)
  }
  
  def length2[A](l : Liste[A])=
    foldRight(l,0)(( _ , x: Int) => x + 1)
  
  //Ex 10 : https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/datastructures/List.scala
  @annotation.tailrec  
  def foldLeft [A,B](l: Liste[A], z:B)(f: (B,A) => B): B = l match {
      case Vide => z
      case SSL(x,xs) => foldLeft(xs,f(z,x))(f)
  }
  
  //Ex 11
  def sumLeft(l: Liste[Int])=
    foldLeft(l,0)(_+_)
    
  def productLeft(l: Liste[Double])=
    foldLeft(l,1.0)(_*_) 
    
  def lengthLeft[A](l : Liste[A])=
    foldLeft(l,0)(( x: Int, _ ) => x + 1)
  
  //Ex 12
  def reverse[A](l : Liste[A]): Liste[A] = l match {
    case Vide => Vide
    case SSL(h,t) => append(reverse(t),SSL(h,Vide))
  }
  // http://oldfashionedsoftware.com/2009/07/30/lots-and-lots-of-foldleft-examples/
  def reverseLeft[A](l : Liste[A]): Liste[A] = 
    foldLeft(l,Liste[A]())((r,c) => SSL(c,r))
 
  
}
