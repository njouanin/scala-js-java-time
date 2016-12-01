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
import java.time.{temporal, _}
import java.time.temporal._
import java.util.{Comparator, Objects}

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
    if (query == TemporalQueries.chronology()) {
      return getChronology().asInstanceOf[R]
    } else if (query == TemporalQueries.precision()) {
      return ChronoUnit.NANOS.asInstanceOf[R]
    } else if (query == TemporalQueries.localDate()) {
      return LocalDate.ofEpochDay(toLocalDate().toEpochDay()).asInstanceOf[R]
    } else if (query == TemporalQueries.localTime()) {
      return toLocalTime().asInstanceOf[R]
    } else if (query == TemporalQueries.zone() || query == TemporalQueries
                 .zoneId() || query == TemporalQueries.offset()) {
      return null.asInstanceOf[R]
    }
    return super.query(query);
  }

  override def adjustInto(temporal: Temporal): Temporal = {
    val date: ChronoLocalDate = toLocalDate()
    temporal
      .`with`(ChronoField.EPOCH_DAY, date.toEpochDay())
      .`with`(ChronoField.NANO_OF_DAY, toLocalTime().toNanoOfDay())

  }

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
    val date: ChronoLocalDate = toLocalDate()
    val oDate = other.toLocalDate()
    var cmp = date.compareTo(oDate.asInstanceOf[D])
    if (cmp == 0) {
      cmp = toLocalTime().compareTo(other.toLocalTime())
      if (cmp == 0) {
        cmp = getChronology().compareTo(other.getChronology())
      }
    }
    cmp
  }

  def isAfter(other: ChronoLocalDateTime[_]): Boolean = {
    val date: ChronoLocalDate = this.toLocalDate()
    val thisEpDay = date.toEpochDay()
    val oDate = other.toLocalDate()
    val otherEpDay = oDate.asInstanceOf[D].toEpochDay()
    thisEpDay > otherEpDay ||
    (thisEpDay == otherEpDay && this.toLocalTime().toNanoOfDay() > other
      .toLocalTime()
      .toNanoOfDay())
  }
  def isBefore(other: ChronoLocalDateTime[_]): Boolean = {
    val thisEpDay = this.toLocalDate().toEpochDay()
    val otherEpDay = other.toLocalDate().asInstanceOf[D].toEpochDay()
    thisEpDay < otherEpDay ||
    (thisEpDay == otherEpDay && this.toLocalTime().toNanoOfDay() < other
      .toLocalTime()
      .toNanoOfDay())
  }

  def isEqual(other: ChronoLocalDateTime[_]): Boolean = {
    this.toLocalTime().toNanoOfDay() == other.toLocalTime().toNanoOfDay() &&
    this.toLocalDate().asInstanceOf[D].toEpochDay() == other
      .toLocalDate()
      .asInstanceOf[D]
      .toEpochDay()
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

  override def hashCode: Int =
    toLocalDate().hashCode() ^ toLocalTime().hashCode()

  override def toString(): String =
    toLocalDate().toString() + 'T' + toLocalTime().toString()

}

object ChronoLocalDateTime {
  def timeLineOrder(): Comparator[ChronoLocalDateTime[_]] =
    DATE_TIME_COMPARATOR

  private val DATE_TIME_COMPARATOR: Comparator[ChronoLocalDateTime[_]] =
    new Comparator[ChronoLocalDateTime[_]]() {
      override def compare(datetime1: ChronoLocalDateTime[_],
                           datetime2: ChronoLocalDateTime[_]): Int = {
        datetime1.toLocalDate().asInstanceOf[ChronoLocalDate].toEpochDay()
        var cmp = Utils.compareLongs(
          datetime1.toLocalDate().asInstanceOf[ChronoLocalDate].toEpochDay(),
          datetime2.toLocalDate().asInstanceOf[ChronoLocalDate].toEpochDay())
        if (cmp == 0) {
          cmp = Utils.compareLongs(datetime1.toLocalTime().toNanoOfDay(),
                                   datetime2.toLocalTime().toNanoOfDay())
        }
        cmp
      }
    }

  def from(temporal: TemporalAccessor): ChronoLocalDateTime[_] = {
    Objects.requireNonNull(temporal, "temporal")
    if (temporal.isInstanceOf[ChronoLocalDateTime[_]]) {
      temporal.asInstanceOf[ChronoLocalDateTime[_]]
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
