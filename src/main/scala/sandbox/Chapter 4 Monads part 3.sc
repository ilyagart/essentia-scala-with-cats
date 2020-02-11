
import cats.Foldable
import cats.data.State
import cats.kernel.{Monoid, Semigroup}
val a = State[Int, String] { state =>
  (state, s"The state is $state")
}
// a: cats.data.State[Int,String] = cats.data.IndexedStateT@6cca9e53

// Get the state and the result:
val (state, result) = a.run(10).value
// state: Int = 10
// result: String = The state is 10
// Get the state, ignore the result:
val state = a.runS(10).value
// state: Int = 10
// Get the result, ignore the state:
val result = a.runA(10).value
// result: String = The state is 10

val step1 = State[Int, String] { num =>
  val ans = num + 1
  (ans, s"Result of step1: $ans")
}
// step1: cats.data.State[Int,String] = cats.data.IndexedStateT@e2d98c
val step2 = State[Int, String] { num =>
  val ans = num * 2
  (ans, s"Result of step2: $ans")
}
// step2: cats.data.State[Int,String] = cats.data.
//IndexedStateT@5982d592
val both = for {
  a <- step1
  b <- step2
} yield (a, b)
// both: cats.data.IndexedStateT[cats.Eval,Int,Int,(String, String)] =
//cats.data.IndexedStateT@2d336af6
val (state, result) = both.run(20).value
// state: Int = 42
// result: (String, String) = (Result of step1: 21,Result of step2:42)

val getDemo = State.get[Int]
// getDemo: cats.data.State[Int,Int] = cats.data.
//IndexedStateT@5f10cd3e
getDemo.run(10).value
// res3: (Int, Int) = (10,10)
val setDemo = State.set[Int](30)
// setDemo: cats.data.State[Int,Unit] = cats.data.
//IndexedStateT@18654165
setDemo.run(10).value
// res4: (Int, Unit) = (30,())
val pureDemo = State.pure[Int, String]("Result")
// pureDemo: cats.data.State[Int,String] = cats.data.
//IndexedStateT@7da49f73
pureDemo.run(10).value
// res5: (Int, String) = (10,Result)
val inspectDemo = State.inspect[Int, String](_ + "!")
// inspectDemo: cats.data.State[Int,String] = cats.data.
//IndexedStateT@24ad766f
inspectDemo.run(10).value
// res6: (Int, String) = (10,10!)
val modifyDemo = State.modify[Int](_ + 1)
// modifyDemo: cats.data.State[Int,Unit] = cats.data.
//IndexedStateT@3f81d8a3
modifyDemo.run(10).value
// res7: (Int, Unit) = (11,())

import State._
val program: State[Int, (Int, Int, Int)] = for {
  a <- get[Int]
  _ <- set[Int](a + 1)
  b <- get[Int]
  _ <- modify[Int](_ + 1)
  c <- inspect[Int, Int](_ * 1000)
} yield (a, b, c)
// program: cats.data.State[Int,(Int, Int, Int)] = cats.data.IndexedStateT@528336cd
val (state, result) = program.run(1).value
// state: Int = 3
// result: (Int, Int, Int) = (1,2,3000)

type CalcState[A] = State[List[Int], A]

def operand(num: Int): CalcState[Int] =
  State[List[Int], Int] { stack =>
    (num :: stack, num)
  }

def operator(func: (Int, Int) => Int): CalcState[Int] =
  State[List[Int], Int] {
    case b :: a :: tail =>
      val ans = func(a, b)
      (ans :: tail, ans)
    case _ =>
      sys.error("Fail!")
  }

def evalOne(sym: String): CalcState[Int] = sym match {
  case "+" => operator(_ + _)
  case "-" => operator(_ - _)
  case "*" => operator(_ * _)
  case "/" => operator(_ / _)
  case num => operand(num.toInt)
}
evalOne("42").runA(Nil).value
val program2 = for {
  _ <- evalOne("1")
  _ <- evalOne("2")
  _ <- evalOne("+")
  _ <- evalOne("3")
  _ <- evalOne("+")
  _ <- evalOne("4")
  ans <- evalOne("+")
  _ <- evalOne("10")
  _ <- evalOne("/")
} yield ans
// program: cats.data.IndexedStateT[cats.Eval,List[Int],List[Int],Int]
//  = cats.data.IndexedStateT@4275a95c
program2.runA(Nil).value

import cats.syntax.applicative._

def evalAll(input: List[String]): CalcState[Int] =
  input.foldLeft(0.pure[CalcState]) { (a, b) =>
    a.flatMap(_ => evalOne(b))
  }

evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value

(for {
  _ <- evalAll(List("1", "2", "+"))
  _ <- evalAll(List("3", "4", "+"))
  ans <- evalOne("*")
} yield ans).runA(Nil).value

def evalInput(input: String): Int = {
  evalAll(input.split(" ").toList).runA(Nil).value
}
evalInput("1 2 + 3 4 + * 2 *")

import scala.annotation.tailrec
import cats.Monad
val optionMonad: Monad[Option] = new Monad[Option] {
  def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] =
    opt flatMap fn
  def pure[A](opt: A): Option[A] =
    Some(opt)
  @tailrec
  def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
    fn(a) match {
      case None           => None
      case Some(Left(a1)) => tailRecM(a1)(fn)
      case Some(Right(b)) => Some(b)
    }
}

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)
def leaf[A](value: A): Tree[A] =
  Leaf(value)

implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
  def pure[A](value: A): Tree[A] =
    Leaf(value)

  def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
    tree match {
      case Branch(l, r) =>
        Branch(flatMap(l)(func), flatMap(r)(func))
      case Leaf(value) =>
        func(value)
    }

  def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] =
    flatMap(func(a)) {
      case Left(value) =>
        tailRecM(value)(func)
      case Right(value) =>
        Leaf(value)
    }
}

val tree =
  Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(4), Leaf(5)))

optionMonad.pure(Leaf(2))
optionMonad.map(Some(tree))(_.right)
optionMonad.flatMap(Some(tree))(Some(_))

import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap
branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
// res3: wrapper.Tree[Int] = Branch(Branch(Leaf(99),Leaf(101)),Branch(Leaf(199),Leaf(201)))
for {
  a <- branch(leaf(100), leaf(200))
  b <- branch(leaf(a - 10), leaf(a + 10))
  c <- branch(leaf(b - 1), leaf(b + 1))
} yield c
// res4: wrapper.Tree[Int] = Branch(
// Branch(Branch(Leaf(89),Leaf(91)), Branch(Leaf(109),Leaf(111))),
// Branch(Branch(Leaf(189),Leaf(191)), Branch(Leaf(209),Leaf(211)))
// )
