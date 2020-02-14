import cats.implicits._

/**
    * MONOID AND SEMIGROUP
    */
import cats.Monoid
import cats.Semigroup
Monoid[String].combine("Hi ", "there")
// res0: String = Hi there
1 |+| 2 |+| Monoid[Int].empty
// res1: Int = 3
val map1 = Map("a" -> 1, "b" -> 2)
val map2 = Map("b" -> 3, "d" -> 4)
map1 |+| map2
// res2: Map[String,Int] = Map(b -> 5, d -> 4, a -> 1)
Semigroup[Int].combine(10, 32)
// res3: Int = 42

/**
    * FUNCTOR
    */
import cats.Functor
val list1 = List(1, 2, 3)
// list1: List[Int] = List(1, 2, 3)
val list2 = Functor[List].map(list1)(_ * 2)
// list2: List[Int] = List(2, 4, 6)
val option1 = Option(123)
// option1: Option[Int] = Some(123)
val option2 = Functor[Option].map(option1)(_.toString)
// option2: Option[String] = Some(123)
val func = (x: Int) => x + 1
// func: Int => Int = <function1>
val liftedFunc = Functor[Option].lift(func)
// liftedFunc: Option[Int] => Option[Int] = cats.Functor<function>
liftedFunc(Option(1))
// res4: Option[Int] = Some(2)
val func1 = (a: Int) => a + 1
val func2 = (a: Int) => a * 2
val func3 = (a: Int) => a + "!"
//val func4 = func1.map(func2).map(func3)
//func4(123)
// res5: String = 248!
/**
    * MONAD
    */
import cats.Monad
val opt1 = Monad[Option].pure(3)
// opt1: Option[Int] = Some(3)
val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
// opt2: Option[Int] = Some(5)

/**
    * ID MONAD
    */
import cats.Id

def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  for {
    x <- a
    y <- b
  } yield x * x + y * y
// sumSquare(3,4) -> error
sumSquare(3: Id[Int], 4: Id[Int])
// res5: cats.Id[Int] = 25
/**
    * EVAL MONAD
    */
/**
    * WRITER MONAD
    */
/**
    * READER MONAD
    */
/**
    * SEMIGROUPAL
    */
/**
    * APPLICATIVE
    */
/**
    * VALIDATED
    */
/**
    * FOLFDABLE
    */
/**
    * TRAVERSE
    */
/**
    * KLEISLI
    */
import cats.data.Kleisli
import cats.instances.list._ // for Monad

val step1: Kleisli[List, Int, Int] =
  Kleisli(x => List(x + 1, x - 1))
val step2: Kleisli[List, Int, Int] =
  Kleisli(x => List(x, -x))
val step3: Kleisli[List, Int, Int] =
  Kleisli(x => List(x * 2, x / 2))

val pipeline = step1 andThen step2 andThen step3

pipeline.run(20)

val sneakyKleisli: Kleisli[List, String, Int] =
  Kleisli(x => List(x.toInt, x.toInt + 43))

sneakyKleisli.run("-1")