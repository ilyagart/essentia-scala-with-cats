def parseInt(str: String): Option[Int] =
  scala.util.Try(str.toInt).toOption
def divide(a: Int, b: Int): Option[Int] =
  if (b == 0) None else Some(a / b)
def stringDivideBy(aStr: String, bStr: String): Option[Int] =
  parseInt(aStr).flatMap { aNum =>
    parseInt(bStr).flatMap { bNum =>
      divide(aNum, bNum)
    }
  }
// same but clearer
def stringDivideBy2(aStr: String, bStr: String): Option[Int] =
  for {
    aNum <- parseInt(aStr)
    bNum <- parseInt(bStr)
    ans <- divide(aNum, bNum)
  } yield ans

stringDivideBy2("10", "5")
//res0: Option[Int] = Some(2)
stringDivideBy2("6", "2")
// res1: Option[Int] = Some(3)
stringDivideBy2("6", "0")
// res2: Option[Int] = None
stringDivideBy2("6", "foo")
// res3: Option[Int] = None
stringDivideBy2("bar", "2")
// res4: Option[Int] = None

trait Monad[F[_]] {
  def pure[A](value: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(a => pure(func(a)))
}

import scala.language.higherKinds
import cats.Monad
import cats.instances.option._ // for Monad
import cats.instances.list._ // for Monad
val opt1 = Monad[Option].pure(3)
// opt1: Option[Int] = Some(3)
val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
// opt2: Option[Int] = Some(5)
val opt3 = Monad[Option].map(opt2)(a => 100 * a)
// opt3: Option[Int] = Some(500)
val list1 = Monad[List].pure(3)
// list1: List[Int] = List(3)
val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
// list2: List[Int] = List(1, 10, 2, 20, 3, 30)
val list3 = Monad[List].map(list2)(a => a + 123)
// list3: List[Int] = List(124, 133, 125, 143, 126, 153)