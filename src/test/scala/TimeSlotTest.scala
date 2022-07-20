import Solution.TimeSlot
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Try

class TimeSlotTest extends AnyFlatSpec {
  "TimeSlot" should "fail on wrong input" in {
    def shouldFail(from: Int, to: Int): Unit = {
      val shouldError: Try[TimeSlot] = Try(TimeSlot(from, to))
      assert(shouldError.isFailure)
    }
    shouldFail(-1, 0)
    shouldFail(0, 0)
    shouldFail(10, 0)
  }

  it should "succeed on right input" in {
    val shouldSucceed: Try[TimeSlot] = Try(TimeSlot(0, 60))
    assert(shouldSucceed.isSuccess)
    assert(shouldSucceed.get.from == 0 && shouldSucceed.get.to == 60)
  }

  it should "succeed with equals" in {
    val ts1: TimeSlot = TimeSlot(0, 60)
    val ts2: TimeSlot = TimeSlot(0, 60)
    val ts3: TimeSlot = TimeSlot(60, 120)
    assert(ts1.equals(ts2))
    assert(!ts1.equals(ts3))
    assert(!ts2.equals(ts3))
  }

  it should "sort correctly" in {
    val last: TimeSlot = TimeSlot(60, 120)
    val head: TimeSlot = TimeSlot(0, 60)
    val sorted: Array[TimeSlot] = Array(last, head).sorted
    assert(sorted.head == head && sorted.last == last)

  }
}