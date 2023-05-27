# matrix-practice
The idea of the project is to implement mathematical operations on vectors and matrices so that they guarantee correct dimensions for both arguments and return values at compile time.

For example, the prototype of a vector looks like this:
```scala
trait Vector[Size <: Int, +A](val size: Size)(using val sizeEvidence: Evidence[Size > 0])
```
where `Evidence[Size > 0]` is a compiletime guarantee that the vector size is greater than 0. 
And the constructor is built so that we can get a vector parameterized by a specific `Int & Singleton` type from any number of passed arguments: 
```scala
val v3: Vector[3, Char]   = Vector.of('A', 'B', 'C')
val v2: Vector[2, Double] = Vector.of(1.0, 2.0)
```
An incorrect number of arguments will cause a compile-time error:
```scala
[error] 10 |val v1: Vector[1, Int] = Vector.of(1, 2)
[error]    |                                        ^
[error]    |              Cannot prove that Tuple.Size[(?1 : (Int, Int))]
[error]    |
[error]    |              where:    ?1 is an unknown value of type (Int, Int)
[error]    |               =:= (1 : Int).
```

The same works for matrices:
```scala
val m: Matrix[2, 2, Double] = Matrix {
  Vector.of(
    Vector.of(1.0, 2.0),
    Vector.of(3.0, 4.0),
  )
}
```

Consequently, in addition to creating matrices and vectors, we get compiletime checks for all other operations.
For example, adding vectors with different size will not be compiled:
```scala
val v2: Vector[2, Double] = ???
val v3: Vector[3, Double] = ???
v2 + v3
```
the error:
```scala
[error] 10 |  v2 + v3
[error]    |       ^^
[error]    |       Found:    (matrix.test.v3 : matrix.Vector[(3 : Int), Double])
[error]    |       Required: matrix.Vector[(2 : Int), Double]
```
