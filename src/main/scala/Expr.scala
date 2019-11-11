import cats._
import cats.implicits._
import Expr.Sub
import Expr.Add
import Expr.Lit
import Expr.Var

sealed trait Expr[A] extends Product with Serializable

object Expr {
    final case class Add[A](left: A, right: A) extends Expr[A]
    final case class Sub[A](left: A, right: A) extends Expr[A]
    final case class Lit[A](n: Int) extends Expr[A]
    final case class Var[A](v: String) extends Expr[A]

    def add(left: Fix[Expr], right: Fix[Expr]): Fix[Expr] = Fix(Add(left, right))
    def sub(left: Fix[Expr], right: Fix[Expr]): Fix[Expr] = Fix(Sub(left, right))
    def lit(n: Int): Fix[Expr] = Fix(Lit(n))
    def variable(v: String): Fix[Expr] = Fix(Var(v))


    implicit val instancesForExpr: Traverse[Expr] = new Traverse[Expr] {
        override def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = fa match {
            case Sub(left, right) => Sub(f(left), f(right))
            case Add(left, right) => Add(f(left), f(right))
            case Lit(n) => Lit(n)
            case Var(v) => Var(v)
        }

        def foldLeft[A, B](fa: Expr[A], b: B)(f: (B, A) => B): B = ???

        def foldRight[A, B](fa: Expr[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???


        def traverse[G[_]: Applicative, A, B](fa: Expr[A])(f: A => G[B]): G[Expr[B]] = fa match {
            case Add(left, right) => (f(left), f(right)).mapN(Add(_, _))
            case Var(v) => (Var(v): Expr[B]).pure[G]
            case Lit(n) => (Lit(n): Expr[B]).pure[G]
            case Sub(left, right) => (f(left), f(right)).mapN(Sub(_, _))
        }
    }
    
}