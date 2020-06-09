// A monad M is a parametric type M[T] with two operations -- flatMap and unit (that have to satisfy some laws)
//  trait M[T] {
//    def flatMap[U](f: T=> M[U]: M[U])
//  }

// Examples
// List is a monad with unit(x) = List(x)
// Set is a monad with unit(x) = Set(x)
// Option is a monad with unit(x) = Some(x)
// Generator is a monad with unit(x) = single(x)

// Map is defined for every monad as a combination of flatMap and unit
// m map f == m flatMap(f andThen unit)

// Monad laws:
// Associativity:
// m flatMap f flatMap g == m flatMap (x => f(x) flatMap g)
// Left Unit:
// unit(x) flatMap f == f(x)
// Right Unit
// m flatMap unit == m

