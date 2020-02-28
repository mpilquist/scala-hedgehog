package hedgehog

/**
 * We have our own FP predef for 2 reasons.
 *
 * 1. The obvious political reasons. I don't think there are any really good reasons to need more than one
 * implementation of this library (if we do our job correctly).
 *
 * Probably more importantly:
 *
 * 2. Library dependencies _do_ have a cost. Especially in the JVM world where we insist of relying on binary
 * compatibility.
 */
package object predef {

  implicit def EitherOps[L, R](e: Either[L, R]): EitherOps[L, R] =
    new EitherOps(e)

  type State[S, A] = StateT[Identity, S, A]

  def State: StateTOpt[Identity] =
    new StateTOpt[Identity] {}

  def stateT[M[_]]: StateTOpt[M] =
    new StateTOpt[M] {}

  def some[A](a: A): Option[A] =
    Some(a)

  def findMap[F[_], A, B](fa: LazyList[A])(f: A => F[Option[B]])(implicit F: Monad[F]): F[Option[B]] = {
    fa match {
      case LazyList.Nil() => F.point(None)
      case LazyList.Cons(h, t) =>
        F.bind(f(h())) {
          case Some(b) => F.point(Some(b))
          case None => findMap(t())(f)
        }
    }
  }

  /** Performs the action `n` times, returning the list of results. */
  def replicateM[M[_], A](n: Int, fa: M[A])(implicit F: Applicative[M]): M[List[A]] =
    sequence(List.fill(n)(fa))

  /** Strict sequencing in an applicative functor `M` that ignores the value in `fa`. */
  def sequence[M[_], A](fa: List[M[A]])(implicit F: Applicative[M]): M[List[A]] =
    traverse(fa)(identity)

  def traverse[M[_], A, B](fa: List[A])(f: A => M[B])(implicit F: Applicative[M]): M[List[B]] =
    fa match {
      case Nil =>
        F.point(Nil)
      case h :: t =>
        F.ap(traverse(t)(f))(F.ap(f(h))(F.point((h2 : B) => (t2 : List[B]) => h2 :: t2)))
    }

  def traverseOpt[M[_], A, B](fa: Option[A])(f: A => M[B])(implicit F: Applicative[M]): M[Option[B]] =
    fa match {
      case None => F.point(None)
      case Some(a) => F.map(f(a))(Some(_))
    }
}
