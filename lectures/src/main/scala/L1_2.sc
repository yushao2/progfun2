// Translation of for

// for loops are implemented using filter, map & flatmap
// e.g.

// for (x <- e1 if f; s) yield e2
// is translated to:
// for (x <- e1.withFilter(x => f); s) yield e2

// for (x <- e1; y <- e2; s) yield e3
// is translated to:
// e1.flatMap( x=> for (y <- e2; s) yield e3)
// (And the translation continues with the new expression)

// Exercise: Translate
for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title
// Translated
books flatMap(b => b.authors.withFilter(a=>a startsWith "Bird").map(y=>y.title))

// Generalization of for -> for is not limited to lists/ seq or collections.
// it is based on the presence of the methods "map", "flatMap" and "withFilter"



