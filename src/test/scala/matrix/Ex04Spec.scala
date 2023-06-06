package matrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.LinearInterpolation.given

/*
 * let u = Vector::from([0., 0., 0.]);
println!("{}, {}, {}, {}", u.norm_1(), u.norm(), u.norm_inf());
// 0.0, 0.0, 0.0
let u = Vector::from([1., 2., 3.]);
println!("{}, {}, {}, {}", u.norm_1(), u.norm(), u.norm_inf());
// 6.0, 3.74165738, 3.0
let u = Vector::from([-1., -2.]);
println!("{}, {}, {}, {}", u.norm_1(), u.norm(), u.norm_inf());
// 3.0, 2.236067977, 2.0*/
class Ex04Spec extends AnyFlatSpec with Matchers:
  private val v1 = Vector.of(0.0, 0.0, 0.0)
  private val v2 = Vector.of(1.0, 2.0, 3.0)
  private val v3 = Vector.of(-1.0, -2.0)

  "Vector.norm1" should "return the sum of elements" in {
    v1.norm1 shouldBe 0.0
    v2.norm1 shouldBe 6.0
    v3.norm1 shouldBe 3.0
  }
