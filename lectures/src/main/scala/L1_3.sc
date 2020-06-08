object L1_3
{

  trait Generator[+T] {

    self => // an alias for "this" -- object

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate) // if just this.generate, it's just a recursive call to itself -- infinite loop
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }


  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random

    def generate = rand.nextInt()
  }


  val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

  def pairs[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T, U)] {
    def generate = (t.generate, u.generate)
  }

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo)

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  // More structured type randoms

  // lists
  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = single(Nil)


  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail


  // tree
  trait Tree
  case class Leaf(x: Int) extends Tree
  case class Inner(left: Tree, right:Tree) extends Tree

  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)

  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l,r)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  }yield tree


  // Random test cases
  def test[T] (r: Generator[T], noTimes: Int = 100)(test: T => Boolean): Unit = {
    for (_ <- 0 until noTimes) {
      val value = r.generate
      assert(test(value), "Test failed for: " + value)
    }
    println("Test passed " +noTimes + " times")
  }

}

L1_3.trees.generate

L1_3.test(L1_3.pairs(L1_3.lists,L1_3.lists)) {
  case (xs, ys) =>(xs ++ ys).length >= xs.length
}



