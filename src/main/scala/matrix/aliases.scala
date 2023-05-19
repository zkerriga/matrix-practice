package matrix

import scala.compiletime.ops.int.*

infix type ?<=[A <: Int, B <: Int] = (A <= B) =:= true
infix type ?>=[A <: Int, B <: Int] = (A >= B) =:= true
infix type ?>[A <: Int, B <: Int]  = (A > B) =:= true
infix type ?<[A <: Int, B <: Int]  = (A < B) =:= true
