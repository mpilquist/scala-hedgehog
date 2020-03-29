package hedgehog.core

import hedgehog._
import hedgehog.predef._

/**
 * Generator for random values of `A`.
 */
case class GenT[A](run: (Size, Seed) => Tree[(Seed, Option[A])]) {

  def map[B](f: A => B): GenT[B] =
    GenT((size, seed) => run(size, seed).map(t => t.copy(_2 = t._2.map(f))))

  def flatMap[B](f: A => GenT[B]): GenT[B] =
    GenT((size, seed) => run(size, seed).flatMap(x =>
      x._2.fold(Tree.TreeApplicative.point(x.copy(_2 = Option.empty[B])))(a => f(a).run(size, x._1))
    ))

  def mapTree[B](f: Tree[(Seed, Option[A])] => Tree[(Seed, Option[B])]): GenT[B] =
    GenT((size, seed) => f(run(size, seed)))

  /**********************************************************************/
  // Shrinking

  /**
   * Apply a shrinking function to a generator.
   */
  def shrink(f: A => List[A]): GenT[A] =
    mapTree(_.expand(x =>
      x._2.fold(List.empty[(Seed, Option[A])])(a => f(a).map(y => (x._1, Some(y))))
    ))

  /**
   * Throw away a generator's shrink tree.
   */
  def prune: GenT[A] =
    mapTree(_.prune)

  /**********************************************************************/
  // Combinators - Property

  def log(name: Name): PropertyT[A] =
    // TODO Add better render, although I don't really like Show
    forAllWithLog(x => ForAll(name, x.toString))

  def forAll: PropertyT[A] =
    // TODO Add better render, although I don't really like Show
    forAllWithLog(x => x.toString)

  def forAllWithLog(f: A => Log): PropertyT[A] =
    for {
      x <- propertyT.fromGen(this)
      _ <- propertyT.writeLog(f(x))
    } yield x

  // Different from Haskell version, which uses the MonadGen typeclass
  def lift: PropertyT[A] =
    propertyT.fromGen(this)

  /**********************************************************************/
  // Combinators - Size

  /**
   * Override the size parameter. Returns a generator which uses the given size
   * instead of the runtime-size parameter.
   */
  def resize(size: Size): GenT[A] =
    if (size.value < 0)
      sys.error("Hedgehog.Random.resize: negative size")
    else
      GenT((_, seed) => run(size, seed))

  /**
   * Adjust the size parameter by transforming it with the given function.
   */
  def scale(f: Size => Size): GenT[A] =
    Gen.sized(n => resize(f(n)))

  /**
   * Make a generator smaller by scaling its size parameter.
   */
  def small: GenT[A] =
    scale(_.golden)

  /**********************************************************************/
  // Combinators - Conditional

  /**
   * Discards the generator if the generated value does not satisfy the predicate.
   */
  def ensure(p: A => Boolean): GenT[A] =
    this.flatMap(x => if (p(x)) Gen.constant(x) else Gen.discard)

  /**
   * Generates a value that satisfies a predicate.
   *
   * We keep some state to avoid looping forever.
   * If we trigger these limits then the whole generator is discarded.
   */
  def filter(p: A => Boolean): GenT[A] =
    Gen.filter(this)(p)

  /**
   * Generates a value that satisfies a predicate.
   *
   * Equivalent to `filter` and is used in for-comprehensions.
   */
  def withFilter(p: A => Boolean): GenT[A] =
    filter(p)

  /**********************************************************************/
  // Combinators - Collections

  /** Generates a 'None' some of the time. */
  def option: GenT[Option[A]] =
    Gen.sized(size =>
      Gen.frequency1(
        2 -> Gen.constant(Option.empty[A])
      , 1 + size.value -> this.map(some)
      )
    )

  /** Generates a list using a 'Range' to determine the length. */
  def list(range: Range[Int]): GenT[List[A]] =
    Gen.list(this, range)
}

abstract class GenImplicits1 {

  implicit def GenFunctor: Functor[GenT] =
    new Functor[GenT] {
      override def map[A, B](fa: GenT[A])(f: A => B): GenT[B] =
        fa.map(f)
    }
}

abstract class GenImplicits2 extends GenImplicits1 {

  implicit def GenApplicative: Applicative[GenT] =
    new Applicative[GenT] {
      def point[A](a: => A): GenT[A] =
        GenT((_, s) => Tree.TreeApplicative.point((s, Some(a))))
      override def ap[A, B](fa: => GenT[A])(f: => GenT[A => B]): GenT[B] =
        GenT((size, seed) => {
          val f2 = f.run(size, seed)
          // FIXME: This is not stack safe.
          val fa2 = fa.run(size, f2.value._1)
          Applicative.zip(fa2, f2).map { case ((seed2, oa), (_, o)) =>
            (seed2, o.flatMap(y => oa.map(y(_))))
          }
        })
    }
}

object GenT extends GenImplicits2 {

  implicit def GenMonad: Monad[GenT] =
    new Monad[GenT] {

     override def map[A, B](fa: GenT[A])(f: A => B): GenT[B] =
       fa.map(f)

     override def point[A](a: => A): GenT[A] =
       GenApplicative.point(a)

     override def ap[A, B](fa: => GenT[A])(f: => GenT[A => B]): GenT[B] =
       GenApplicative.ap(fa)(f)

      override def bind[A, B](fa: GenT[A])(f: A => GenT[B]): GenT[B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => GenT[Either[A, B]]): GenT[B] = {
        case class Node[C](value: C, childrenCount: Int)
        @annotation.tailrec
        def loop(size: Size, remaining: LazyList[Tree[(Seed, Option[Either[A, B]])]], stack: List[Node[(Seed, Option[B])]]): Tree[(Seed, Option[B])] = {
          remaining match {
            case LazyList.Cons(head, tail) =>
              val hd = head()
              val (seed, oeab) = hd.value
              oeab match {
                case Some(Left(a)) =>
                  loop(size, LazyList.cons(f(a).run(size, seed), hd.children.value) ++ tail(), stack)
                case Some(Right(b)) =>
                  loop(size, hd.children.value ++ tail(), Node((seed, Some(b): Option[B]), hd.children.value.toList(Int.MaxValue).size) :: stack)
                case None =>
                  loop(size, hd.children.value ++ tail(), Node((seed, None: Option[B]), hd.children.value.toList(Int.MaxValue).size) :: stack)
              }
            case LazyList.Nil() =>
              @annotation.tailrec
              def build(stack: List[Node[(Seed, Option[B])]], out: List[Tree[(Seed, Option[B])]]): Tree[(Seed, Option[B])] = {
                stack match {
                  case Node(v, size) :: tail =>
                    val t = Tree(v, Identity(LazyList.fromList(out.take(size))))
                    build(tail, t :: out.drop(size))
                  case Nil => 
                    out.head
                }
              }
              build(stack, Nil)
          }
        }
        GenT[B]((size, seed) => loop(size, LazyList.cons(f(a).run(size, seed), LazyList.nil), Nil))
      }
    }
}
