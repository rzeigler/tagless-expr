import cats.data._
import cats.implicits._

trait Deref[F[_]] {
    def deref(s: String): F[Int]
    def lit(n: Int): F[Int]
    def add(l: Int, r: Int): F[Int]
    def sub(l: Int, r: Int): F[Int]
}

object Deref {
    implicit val impl: Deref[ReaderT[Option, Map[String, Int], *]] = new Deref[ReaderT[Option, Map[String, Int], *]] {
        def deref(s: String): Kleisli[Option,Map[String,Int],Int] = Kleisli(e => e.get(s))
        def lit(n: Int): Kleisli[Option,Map[String,Int],Int] = Kleisli(_ => n.some)
        def add(l: Int, r: Int): Kleisli[Option,Map[String,Int],Int] = Kleisli.pure(l + r)
        def sub(l: Int, r: Int): Kleisli[Option,Map[String,Int],Int] = Kleisli.pure(l - r)
    }
}