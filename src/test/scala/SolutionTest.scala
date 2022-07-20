import Solution.{TimeSlot, suggestTime}
import org.scalatest.flatspec.AnyFlatSpec

class SolutionTest extends AnyFlatSpec {

  private def validate(input: Array[TimeSlot],
                       expected: Array[TimeSlot],
                       eventLength: Int): Unit = {
    val result: Array[TimeSlot] = suggestTime(input, eventLength)
    result.indices.foreach((i: Int) =>
      assert(result(i) == expected(i))
    )
  }

  "Solution" should "succeed with 60 minute window" in {
    //    val input1: Array[TimeSlot] = Array(TimeSlot(0, 59), TimeSlot(120, 179))
    //    val expected1: Array[TimeSlot] = Array(TimeSlot(60, 119), TimeSlot(180, 1439))
    //    validate(input1, expected1)
    //  }
    //  it should "succeed with 30 minute window" in {
    val input2: Array[TimeSlot] = Array(TimeSlot(0, 29), TimeSlot(120, 179))
    val expected2: Array[TimeSlot] = Array(TimeSlot(30, 119), TimeSlot(150, 1439))
    validate(input2, expected2, eventLength = 30)
  }
}
