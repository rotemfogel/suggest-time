import scala.annotation.tailrec

object Solution {
  case class TimeSlot(from: Int, to: Int) extends Ordered[TimeSlot] {
    require(0 <= from &&
      from < 1440 &&
      from < to)

    @inline override def equals(obj: Any): Boolean = {
      obj match {
        case other: TimeSlot =>
          from.equals(other.from) &&
            to.equals(other.to)
        case _ => false
      }
    }

    private def value: Int = from + to

    @inline override def compare(that: TimeSlot): Int = {
      if (this.value < that.value) -1
      else if (this.value == that.value) 0
      else 1
    }
  }


  def adjacentFn(list: List[TimeSlot]): List[TimeSlot] = {
    /**
     * calculate the correct accumulator
     */
    val calculateAcc: (List[TimeSlot], List[TimeSlot]) =>
      List[TimeSlot] = (s: List[TimeSlot], acc: List[TimeSlot]) => {
      if (acc.isEmpty)
        acc :+ TimeSlot(s.head.from, s.tail.head.to)
      // if last accumulated timeslot is adjacent to current timeslot
      // TODO: fix for 30 minute window
      else if (acc.last.to == s.head.from - 1 ||
        acc.last.to == s.head.to) {
        val lastTimeSlot: TimeSlot = acc.last
        val to: Int = if (s.tail.nonEmpty) s.tail.head.from - 1
        else s.head.to
        acc.slice(0, acc.length - 1) :+ lastTimeSlot.copy(to = to)
      }
      else acc :+ s.head
    }

    /**
     * tail recursive function to collapse adjacent time slots
     *
     * @param s   - the list of timeslots
     * @param acc - the accumulator
     * @return list of timeslots
     */
    @tailrec
    def adjacentRecFn(s: List[TimeSlot], acc: List[TimeSlot]): List[TimeSlot] = {
      if (s.isEmpty) acc
      else adjacentRecFn(s.tail, calculateAcc(s, acc))
    }

    adjacentRecFn(list, List.empty[TimeSlot])
  }


  /**
   * Return all open slots for a scheduled meeting.
   * A time slot is an object that represents a time range.
   * Each time range object contains a from and to properties
   * which is a numeric representation of a minute within a day (a number between 0 and 1439)
   *
   * @param busyRanges  - An array of busy slots.
   * @param eventLength - A number that represents the length of the event.
   * @return An array of free slots which the meeting can occur.
   *         For example:
   *         suggestTime([{from: 0, to:59}, {from: 120, to: 179}], 60) - return [{from: 60, to: 119}, {from: 180, to: 1439}]
   */
  def suggestTime(busyRanges: Array[TimeSlot], eventLength: Int): Array[TimeSlot] = {
    // sort the ranges and get the start hours
    val start: Array[Int] = busyRanges.map((_: TimeSlot).from)
    // build the list of available ranges
    val interim: List[TimeSlot] = Range.inclusive(0, 1440, eventLength)
      .takeWhile((_: Int) < 1440)
      .map((r: Int) => TimeSlot(r, r + eventLength - 1))
      .filterNot((r: TimeSlot) => start.contains(r.from))
      .toList
    // find adjacent time slots
    adjacentFn(interim)
      .toArray
  }
}