import Expr.Add
import Expr.Lit
import Expr.Sub
import Expr.Var

import cats._
import cats.implicits._
import cats.data._

object Main {


  val aPlus1 = Expr.add(Expr.variable("a"), Expr.lit(1))

  val bMinus2 = Expr.sub(Expr.variable("b"), Expr.lit(2))

  def evalM[M[_]: Deref: Applicative](f: Expr[Int]): M[Int] = 
    f match {
      case Add(left, right) => implicitly[Deref[M]].add(left, right)
      case Lit(n) => implicitly[Deref[M]].lit(n)
      case Sub(left, right) => implicitly[Deref[M]].sub(left, right)
      case Var(v) => implicitly[Deref[M]].deref(v)
    }


  def evalMId(f: Expr[Int]): Id[Int] = f match {
      case Add(left, right) => left + right
      case Lit(n) => n
      case Sub(left, right) => left - right
      case Var(v) => 0
  }
  
  type M[A] = Kleisli[Option, Map[String, Int], A]

  val input = Map("a" -> 42)
  def main(args: Array[String]): Unit = {
    println(aPlus1.cataM[M, Int](evalM(_)).run(input))
    println(bMinus2.cataM[M, Int](evalM(_)).run(input))

    println(aPlus1.cataM[Id, Int](evalMId(_)))
    println(bMinus2.cataM[Id, Int](evalMId(_)))
   }
}