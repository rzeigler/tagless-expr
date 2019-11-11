import cats._
import cats.implicits._

final case class Fix[F[_]](unFix: F[Fix[F]]) {
    def cata[A](f: F[A] => A)(implicit F: Functor[F]): A = 
        f(unFix.map(_.cata(f)))

    def cataM[M[_], A](f: F[A] => M[A])(implicit T: Traverse[F], F: Functor[F], M: Monad[M]): M[A] =  {
        val a = unFix.map(_.cataM(f))
        a.sequence.flatMap(f)
    }
}