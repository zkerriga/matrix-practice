package matrix

object TupleUtils {
  type Size[X <: Tuple] <: Int & Singleton = X match {
    case EmptyTuple => 0
    case x *: xs    => SingletonIntPlus[Size[xs], 1]#Result
  }
}
