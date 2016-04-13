import org.scalatest._


class StripSpec extends FlatSpec with Matchers {
// See manual here http://doc.scalatest.org/1.8/org/scalatest/FlatSpec.html

  "A Sample object" should "do nothing" in {
    val sample = new Strip[String](7, x => x.toString.toLong)
    sample should be ( sample )
    val myMarker1 = sample.add(List("100", "200", "300", "400"))
    val myMarker2 = sample.add(List("200", "300", "700", "800"))
    val myMarker3 = sample.add(List())
    val myMarker4 = sample.add(List("900", "1000", "1100", "1200", "100"))
    val myMarker = sample.add(List("1300", "1400", "1500", "1600", "700", "500", "400", "1700"))
    println("current marker: " + myMarker)
    println("store size: " + sample.store.size)
    println("store elemets: " + sample.store)
    val ret1 = sample.getUp(3,11)
    val ret2 = sample.getUp(3,7)
    val ret3 = sample.getUp(12,7)
    val ret4 = sample.getUp(23,18)
    val ret5 = sample.getUp(3,18)

    val ret11 = sample.getDown(3,11)
    val ret12 = sample.getDown(3,7)
    val ret13 = sample.getDown(12,7)
    val ret14 = sample.getDown(23,18)
    val ret15 = sample.getDown(3,18)

  }
/**
  it should "do nothing else" in {
    val sample = new Strip(10)
    sample should be ( sample )
    val myMarker = sample.add(List("500", "600", "700", "800"))
  }
 */
}
