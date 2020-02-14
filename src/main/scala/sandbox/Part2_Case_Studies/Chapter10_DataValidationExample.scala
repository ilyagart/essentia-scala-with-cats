package sandbox.Part2_Case_Studies

import cats.data.Kleisli

object Chapter10_DataValidationExample extends App {
  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._
  import cats.implicits._

  sealed trait Predicate[E, A] {
    import Predicate._

    def run(implicit s: Semigroup[E]): A => Either[E, A] =
      (a: A) => this(a).toEither

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = {
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(a) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a)    => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
    }
  }
  object Predicate {
    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
        extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
        extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A])
        extends Predicate[E, A]
    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)
    def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
      Pure(a => if (fn(a)) a.valid else err.invalid)
  }

//  sealed trait Check[E, A, B] {
//    import Check._
//
//    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]
//
//    def map[C](func: B => C): Check[E, A, C] = Map[E, A, B, C](this, func)
//
//    def flatMap[C](f: B => Check[E, A, C]): FlatMap[E, A, B, C] =
//      FlatMap[E, A, B, C](this, f)
//
//    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
//      AndThen[E, A, B, C](this, that)
//  }
//
//  object Check {
//    final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C)
//        extends Check[E, A, C] {
//
//      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
//        check(a) map func
//    }
//
//    final case class FlatMap[E, A, B, C](check: Check[E, A, B],
//                                         func: B => Check[E, A, C])
//        extends Check[E, A, C] {
//
//      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
//        check(a).withEither(_.flatMap(b => func(b)(a).toEither))
//    }
//
//    final case class AndThen[E, A, B, C](check1: Check[E, A, B],
//                                         check2: Check[E, B, C])
//        extends Check[E, A, C] {
//      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
//        check1(a).withEither(_.flatMap(b => check2(b).toEither))
//    }
//
//    final case class Pure[E, A, B](func: A => Validated[E, B])
//        extends Check[E, A, B] {
//
//      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] = func(a)
//    }
//
//    final case class PurePredicate[E, A](pred: Predicate[E, A])
//        extends Check[E, A, A] {
//
//      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
//        pred(a)
//    }
//
//    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)
//
//    def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] = Pure(func)
//  }

  import cats.data.NonEmptyList
  type Errors = NonEmptyList[String]
  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)
  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.length > n
    )
  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit)
    )
  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char)
    )
  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.count(c => c == char) == 1
    )

  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]
  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)
  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)

  val checkUsername: Check[String, String] = checkPred(
    longerThan(3) and alphanumeric
  )

  def splitEmail: Check[String, (String, String)] =
    check(_.split('@') match {
      case Array(name, domain) => Right((name, domain))
      case _ =>
        Left(error("Must contain a single @ character"))
    })

  val checkLeft: Check[String, String] = checkPred(longerThan(0))

  val checkRight: Check[String, String] = checkPred(
    longerThan(3) and contains('.')
  )

  val joinEmail: Check[(String, String), String] = check {
    case (l, r) => (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
  }

  val checkEmail: Check[String, String] = splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Either[Errors, User] =
    (checkUsername.run(username), checkEmail.run(email)).mapN(User)

  println(createUser("Noel", "noel@underscore.io"))
  // res14: cats.data.Validated[wrapper.Errors,User] = Valid(User(Noel,
  //  noel@underscore.io))
  println(createUser("", "dave@underscore@io"))
  // res15: cats.data.Validated[wrapper.Errors,User] = Invalid(
  //  NonEmptyList(Must be longer than 3 characters, Must contain a
  //    single @ character))

}
