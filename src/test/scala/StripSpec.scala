import org.scalatest._


class StripSpec extends FlatSpec with Matchers {
// See manual here http://doc.scalatest.org/1.8/org/scalatest/FlatSpec.html

  "A Sample object" should "do nothing" in {
    val sample = new Sample()
    sample should be ( sample )
  }

  it should "do nothing else" in {
    val sample = new Sample()
    sample should be ( sample )
  }
}
