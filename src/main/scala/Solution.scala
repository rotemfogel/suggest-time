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

    lazy val range: Int = (to - from) + 1
    private lazy val value: Int = from + to

    @inline override def compare(that: TimeSlot): Int = {
      if (this.value < that.value) -1
      else if (this.value == that.value) 0
      else 1
    }
  }


  def collapseAdjacent(list: List[TimeSlot]): List[TimeSlot] = {
    /**
     * calculate the correct accumulator
     */
    def calculateAcc: (List[TimeSlot], List[TimeSlot]) =>
      List[TimeSlot] = (s: List[TimeSlot], acc: List[TimeSlot]) => {
      if (acc.isEmpty)
        List(TimeSlot(s.head.from, s.head.to))
      // if last accumulated timeslot is adjacent to current timeslot
      else if (acc.last.to == s.head.from - 1 || acc.last.to == s.head.to)
        acc.slice(0, acc.length - 1) :+ acc.last.copy(to = s.head.to)
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
    val adjustedRanges: Array[TimeSlot] = busyRanges.flatMap((b: TimeSlot) => {
      // if range is larger than eventLength, split it to chunks
      if (b.range > eventLength) {
        val upperLimit: Int = b.to + 1
        Range.inclusive(b.from, upperLimit, eventLength)
          .takeWhile((_: Int) < upperLimit)
          .map((r: Int) => TimeSlot(r, r + eventLength - 1))
          .toList
      } else List(b)
    })
    val start: Array[Int] = adjustedRanges.map((_: TimeSlot).from)
    // build the list of available ranges
    val availableSlots: List[TimeSlot] = Range.inclusive(0, 1440, eventLength)
      .takeWhile((_: Int) < 1440)
      .map((r: Int) => TimeSlot(r, r + eventLength - 1))
      .filterNot((r: TimeSlot) => start.contains(r.from))
      .toList
    // find adjacent time slots
    collapseAdjacent(availableSlots)
      .toArray
  }
}