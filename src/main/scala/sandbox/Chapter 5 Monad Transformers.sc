import cats.data.OptionT

type ListOption[A] = OptionT[List, A]
import cats.Monad
import cats.instances.list._ // for Monad
import cats.syntax.applicative._ // for pure
val result1: ListOption[Int] = OptionT(List(Option(10)))
// result1: ListOption[Int] = OptionT(List(Some(10)))
val result2: ListOption[Int] = 32.pure[ListOption]
// result2: ListOption[Int] = OptionT(List(Some(32)))
result1.flatMap { x: Int =>
  result2.map { y: Int =>
    x + y
  }
}
// res1: cats.data.OptionT[List,Int] = OptionT(List(Some(42)))
