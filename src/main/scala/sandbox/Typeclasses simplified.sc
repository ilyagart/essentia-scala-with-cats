//import cats.{Functor, Semigroupal}
trait Semigroup[A] {
  def combine(x: A, y: A): A
}
trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
trait Semigroupal[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}
trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)
}
trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
}
trait Traverse[F[_]] {
  def traverse[G[_]: Applicative, A, B](inputs: F[A])(func: A => G[B]): G[F[B]]
  def sequence[G[_]: Applicative, B](inputs: F[G[B]]): G[F[B]] =
    traverse(inputs)(identity)
}
trait Monad[F[_]] {
  def pure[A](value: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(a => pure(func(a)))
}
