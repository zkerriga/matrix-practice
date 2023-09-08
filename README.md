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

---

In addition to simple operations, the project contains complex algorithms that are implemented using only the public matrix and vector API, which shows how proofs and compiletime checks can work. See [LaplaceExpansion.scala](./src/main/scala/matrix/core/determinant/LaplaceExpansion.scala), [GaussianElimination.scala](./src/main/scala//matrix/core/echelon/GaussianElimination.scala).

All functionalities with links to their use cases (tests):
- [x] Addition, Subtraction, Scaling [Ex00](./src/test/scala/matrix/Ex00Spec.scala)
- [x] Linear combination [Ex01](./src/test/scala/matrix/Ex01Spec.scala)
- [x] Linear interpolation [Ex02](./src/test/scala/matrix/Ex02Spec.scala)
- [x] Dot product [Ex03](./src/test/scala/matrix/Ex03Spec.scala)
- [x] Manhattan norm, Euclidean norm, Supremum norm [Ex04](./src/test/scala/matrix/Ex04Spec.scala)
- [x] Cosine of angle between two vectors [Ex05](./src/test/scala/matrix/Ex05Spec.scala)
- [x] Cross product [Ex06](./src/test/scala/matrix/Ex06Spec.scala)
- [x] Linear map and matrix multiplication [Ex07](./src/test/scala/matrix/Ex07Spec.scala)
- [x] Trace [Ex08](./src/test/scala/matrix/Ex08Spec.scala)
- [x] Transponse [Ex09](./src/test/scala/matrix/Ex09Spec.scala)
- [x] Reduced row echelon form via Gaussian Elimination [Ex10](./src/test/scala/matrix/Ex10Spec.scala)
- [x] Determinant via Laplace Expansion [Ex11](./src/test/scala/matrix/Ex11Spec.scala)
- [x] Inverse matrix [Ex12](./src/test/scala/matrix/Ex12Spec.scala)
- [x] Rank [Ex13](./src/test/scala/matrix/Ex13Spec.scala)
- [x] Projection matrix [Ex14](./src/test/scala/matrix/Ex14Spec.scala)
