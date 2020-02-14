// CRDT - Commutative Replicated Data Types a family of data structures
// that can be used to reconcile eventually consistent data.

import cats.implicits._
import cats.kernel.CommutativeMonoid

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intInstance: BoundedSemiLattice[Int] =
    new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int =
        a1 max a2

      def empty =
        0
    }
  implicit def setInstance[A]: BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]) = a1 union a2

      def empty = Set.empty[A]
    }
}

//final case class GCounter[A](counters: Map[String, A]) {
//  def increment(machine: String,
//                amount: A)(implicit m: CommutativeMonoid[A]): GCounter[A] = {
//    val value = amount |+| counters.getOrElse(machine, m.empty)
//    GCounter(counters + (machine -> value))
//  }
//
//  def merge(
//    that: GCounter[A]
//  )(implicit b: BoundedSemiLattice[A]): GCounter[A] = {
//    GCounter(this.counters |+| that.counters)
//  }
//
//  def total(implicit m: CommutativeMonoid[A]): A =
//    this.counters.values.toList.combineAll
//
//}

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(
    implicit m: CommutativeMonoid[V]
  ): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(
    implicit b: BoundedSemiLattice[V]
  ): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](
    implicit counter: GCounter[F, K, V]
  ): GCounter[F, K, V] =
    counter
}

//implicit def mapInstance[K, V]: GCounter[Map, K, V] =
//  new GCounter[Map, K, V] {
//    override def increment(
//      map: Map[K, V]
//    )(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
//      val total = map.getOrElse(k, m.empty) |+| v
//      map + (k -> total)
//    }
//
//    override def merge(f1: Map[K, V], f2: Map[K, V])(
//      implicit b: BoundedSemiLattice[V]
//    ): Map[K, V] = {
//      f1 |+| f2
//    }
//
//    override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V = {
//      f.values.toList.combineAll
//    }
//  }
//val g1 = Map("a" -> 7, "b" -> 3)
//val g2 = Map("a" -> 2, "b" -> 5)
//val counter = GCounter[Map, String, Int]
//val merged = counter.merge(g1, g2)
//// merged: Map[String,Int] = Map(a -> 7, b -> 5)
//val total = counter.total(merged)
//// total: Int = 12

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)
  def values[K, V](f: F[K, V]): List[V]
}

implicit val mapInstance: KeyValueStore[Map] = new KeyValueStore[Map] {
  def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

  def get[K, V](f: Map[K, V])(k: K): Option[V] = f get k

  override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
    f.getOrElse(k, default)

  def values[K, V](f: Map[K, V]) = f.values.toList
}

implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
  def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
    kvs.put(f)(key, value)
  def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
    kvs.get(f)(key)
  def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
    kvs.getOrElse(f)(key, default)
  def values(implicit kvs: KeyValueStore[F]): List[V] =
    kvs.values(f)
}

implicit def gcounterInstance[F[_, _], K, V](
  implicit kvs: KeyValueStore[F],
  km: CommutativeMonoid[F[K, V]]
): GCounter[F, K, V] =
  new GCounter[F, K, V] {
    def increment(
      f: F[K, V]
    )(key: K, value: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
      val total = f.getOrElse(key, m.empty) |+| value
      f.put(key, total)
    }
    def merge(f1: F[K, V],
              f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
      f1 |+| f2
    def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
      f.values.combineAll
  }

val g1 = Map("a" -> 7, "b" -> 3)
val g2 = Map("a" -> 2, "b" -> 5)
val counter = gcounterInstance[Map, String, Int]
val g1incremented = counter.increment(g1)("c", 4)
val g2incremented = counter.increment(g2)("c", 2)
val merged = counter.merge(g1incremented, g2incremented)
counter.total(merged)