/*
 *********************************************************************************
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <nico@beerfactory.org> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Nicolas JOUANIN
 *********************************************************************************
 */
package java.time.chrono

import java.time.format.DateTimeFormatter
import java.time._
import java.time.temporal._
import java.util.{Comparator, Objects}
import ChronoField._

abstract class ChronoLocalDateTime[D <: ChronoLocalDate]
    extends Temporal
    with TemporalAdjuster
    with Comparable[ChronoLocalDateTime[_]] {

  def getChronology(): Chronology = toLocalDate().getChronology()

  def toLocalDate(): D

  def toLocalTime(): LocalTime

  override def `with`(adjuster: TemporalAdjuster): ChronoLocalDateTime[D] =
    toLocalDate()
      .getChronology()
      .ensureChronoLocalDateTime(super.`with`(adjuster))

  def `with`(field: TemporalField, newValue: Long): ChronoLocalDateTime[D]

  def plus(amount: TemporalAmount): ChronoLocalDateTime[D] =
    toLocalDate().getChronology().ensureChronoLocalDateTime(super.plus(amount))

  def plus(amountToAdd: Long, unit: TemporalUnit): ChronoLocalDateTime[D]

  override def minus(amount: TemporalAmount): ChronoLocalDateTime[D] =
    toLocalDate()
      .getChronology()
      .ensureChronoLocalDateTime(super.minus(amount))

  override def minus(amountToSubtract: Long,
                     unit: TemporalUnit): ChronoLocalDateTime[D] =
    toLocalDate()
      .getChronology()
      .ensureChronoLocalDateTime(super.minus(amountToSubtract, unit))

  override def query[R](query: TemporalQuery[R]): R = {
    query match {
      case TemporalQueries.chronology() ⇒ getChronology()
      case TemporalQueries.precision() ⇒ NANOS;
      case TemporalQueries.localDate() ⇒
        LocalDate.ofEpochDay(toLocalDate().toEpochDay())
      case TemporalQueries.localTime() ⇒ toLocalTime()
      case TemporalQueries.zone() | TemporalQueries.zoneId() |
          TemporalQueries.offset() ⇒
        null
      case _ ⇒ super.query(query)
    }
  }

  override def adjustInto(temporal: Temporal): Temporal =
    temporal
      .`with`(EPOCH_DAY, toLocalDate().toEpochDay())
      .`with`(NANO_OF_DAY, toLocalTime().toNanoOfDay())

  def format(formatter: DateTimeFormatter): String = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.format(this)
  }

  def atZone(zone: ZoneId): ChronoZonedDateTime[D]

  def toInstant(offset: ZoneOffset): Instant =
    Instant.ofEpochSecond(toEpochSecond(offset), toLocalTime().getNano())

  def toEpochSecond(offset: ZoneOffset): Long = {
    Objects.requireNonNull(offset, "offset")
    val epochDay = toLocalDate().toEpochDay()
    (epochDay * 86400 + toLocalTime().toSecondOfDay()) - offset
      .getTotalSeconds()
  }

  override def compareTo(other: ChronoLocalDateTime[_]): Int = {
    var cmp = toLocalDate().compareTo(other.toLocalDate())
    if (cmp == 0) {
      cmp = toLocalTime().compareTo(other.toLocalTime())
      if (cmp == 0) {
        cmp = getChronology().compareTo(other.getChronology())
      }
    }
    cmp
  }

  def isAfter(other: ChronoLocalDateTime[_]): Boolean = {
    val thisEpDay = this.toLocalDate().toEpochDay()
    val otherEpDay = other.toLocalDate().toEpochDay()
    thisEpDay > otherEpDay ||
    (thisEpDay == otherEpDay && this.toLocalTime().toNanoOfDay() > other
      .toLocalTime()
      .toNanoOfDay())
  }
  def isBefore(other: ChronoLocalDateTime[_]): Boolean = {
    val thisEpDay = this.toLocalDate().toEpochDay()
    val otherEpDay = other.toLocalDate().toEpochDay()
    thisEpDay < otherEpDay ||
    (thisEpDay == otherEpDay && this.toLocalTime().toNanoOfDay() < other
      .toLocalTime()
      .toNanoOfDay())
  }

  def isEqual(other: ChronoLocalDateTime[_]): Boolean = {
    this.toLocalTime().toNanoOfDay() == other.toLocalTime().toNanoOfDay() &&
    this.toLocalDate().toEpochDay() == other.toLocalDate().toEpochDay()
  }

  override def equals(obj: Any): Boolean = {
    if (obj == this)
      true
    else {
      obj match {
        case other: ChronoLocalDateTime[_] => compareTo(other) == 0
        case _ => false
      }
    }
  }

  override def hashCode: Int = toLocalDate().hashCode() ^ toLocalTime().hashCode()

  override def toString(): String = toLocalDate().toString() + 'T' + toLocalTime().toString()

}

object ChronoLocalDateTime {
  def timeLineOrder(): Comparator[ChronoLocalDateTime[_]] =
    DATE_TIME_COMPARATOR

  private val DATE_TIME_COMPARATOR: Comparator[ChronoLocalDateTime[_]] =
    new Comparator[ChronoLocalDateTime[_]]() {
      override def compare(datetime1: ChronoLocalDateTime[_],
                           datetime2: ChronoLocalDateTime[_]): Int = {
        datetime1.toLocalDate.toEpochDay()
        var cmp = Jdk8Methods.compareLongs(
          datetime1.toLocalDate().toEpochDay(),
          datetime2.toLocalDate().toEpochDay());
        if (cmp == 0) {
          cmp = Jdk8Methods.compareLongs(
            datetime1.toLocalTime().toNanoOfDay(),
            datetime2.toLocalTime().toNanoOfDay());
        }
        cmp
      }
    }

  def from(temporal: TemporalAccessor): ChronoLocalDateTime[_] = {
    Objects.requireNonNull(temporal, "temporal")
    if (temporal.isInstanceOf[ChronoLocalDateTime]) {
      return temporal.asInstanceOf[ChronoLocalDateTime[_]]
    } else {
      val chrono: Chronology = temporal.query(TemporalQueries.chronology())
      if (chrono == null) {
        throw new DateTimeException(
          "No Chronology found to create ChronoLocalDateTime: " + temporal
            .getClass());
      }
      chrono.localDateTime(temporal)
    }
  }
}
