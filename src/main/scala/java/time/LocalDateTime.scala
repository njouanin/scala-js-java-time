/*
 *********************************************************************************
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <nico@beerfactory.org> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Nicolas JOUANIN
 *********************************************************************************
 */
package java.time

import java.io._
import java.time.chrono.ChronoLocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal._
import java.util.Objects

import java.time.temporal.ChronoField._
import java.time.LocalTime._

class LocalDateTime(private[time] val date: LocalDate,
                    private[time] val time: LocalTime)
    extends ChronoLocalDateTime[LocalDate]
    with Temporal
    with TemporalAdjuster
    with Serializable {

  private def `with`(newDate: LocalDate, newTime: LocalTime): LocalDateTime = {
    if (date == newDate && time == newTime) {
      this
    } else
      new LocalDateTime(newDate, newTime)
  }

  override def isSupported(field: TemporalField): Boolean = {
    field match {
      case _: ChronoField ⇒ field.isDateBased() || field.isTimeBased()
      case _ ⇒ field != null && field.isSupportedBy(this)
    }
  }

  override def isSupported(unit: TemporalUnit): Boolean = {
    unit match {
      case _: ChronoUnit ⇒ unit.isDateBased() || unit.isTimeBased()
      case _ ⇒ unit != null && unit.isSupportedBy(this)
    }
  }

  override def range(field: TemporalField): ValueRange = {
    field match {
      case _: ChronoField ⇒
        if (field.isTimeBased()) time.range(field) else date.range(field)
      case _ ⇒ field.rangeRefinedBy(this)
    }
  }

  override def get(field: TemporalField): Int = {
    field match {
      case _: ChronoField ⇒
        if (field.isTimeBased()) time.get(field) else date.get(field)
      case _ ⇒ super.get(field)
    }
  }

  override def getLong(field: TemporalField): Long = {
    field match {
      case _: ChronoField ⇒
        if (field.isTimeBased()) time.getLong(field) else date.getLong(field)
      case _ ⇒ field.getFrom(this)
    }
  }

  def getYear(): Int = date.getYear()
  def getMonthValue(): Int = date.getMonthValue()
  def getMonth(): Month = date.getMonth()
  def getDayOfMonth(): Int = date.getDayOfMonth()
  def getDayOfYear(): Int = date.getDayOfYear()
  def getDayOfWeek(): DayOfWeek = date.getDayOfWeek()
  def getHour(): Int = time.getHour()
  def getMinute(): Int = time.getMinute()
  def getSecond(): Int = time.getSecond()
  def getNano(): Int = time.getNano()

  override def `with`(adjuster: TemporalAdjuster): LocalDateTime = {
    adjuster match {
      case ld: LocalDate ⇒ `with`(ld, time)
      case lt: LocalTime ⇒ `with`(date, lt)
      case ldt: LocalDateTime ⇒ adjuster.asInstanceOf[LocalDateTime]
      case _ ⇒ adjuster.adjustInto(this).asInstanceOf[LocalDateTime]
    }
  }

  override def `with`(field: TemporalField, newValue: Long): LocalDateTime = {
    field match {
      case _: ChronoField ⇒
        if (field.isTimeBased)
          `with`(date, time.`with`(field, newValue))
        else
          `with`(date.`with`(field, newValue), time)
      case _ ⇒ field.adjustInto(this, newValue)
    }
  }

  def withYear(year: Int): LocalDateTime = `with`(date.withYear(year), time)

  def withMonth(month: Int): LocalDateTime =
    `with`(date.withMonth(month), time)

  def withDayOfMonth(dayOfMonth: Int): LocalDateTime =
    `with`(date.withDayOfMonth(dayOfMonth), time)

  def withDayOfYear(dayOfYear: Int): LocalDateTime =
    `with`(date.withDayOfYear(dayOfYear), time)

  def withHour(hour: Int): LocalDateTime = {
    val newTime = time.withHour(hour)
    `with`(date, newTime)
  }

  def withMinute(minute: Int): LocalDateTime = {
    val newTime = time.withMinute(minute)
    `with`(date, newTime)
  }

  def withSecond(second: Int): LocalDateTime = {
    val newTime = time.withSecond(second)
    `with`(date, newTime)
  }

  def withNano(nanoOfSecond: Int): LocalDateTime = {
    val newTime = time.withNano(nanoOfSecond)
    `with`(date, newTime)
  }

  def truncatedTo(unit: TemporalUnit): LocalDateTime =
    `with`(date, time.truncatedTo(unit))

  override def plus(amount: TemporalAmount): LocalDateTime =
    amount.addTo(this).asInstanceOf[LocalDateTime]

  override def plus(amountToAdd: Long, unit: TemporalUnit): LocalDateTime = {
    unit match {
      case ChronoUnit.NANOS ⇒ plusNanos(amountToAdd)
      case ChronoUnit.MICROS ⇒
        plusDays(amountToAdd / MICROS_PER_DAY)
          .plusNanos((amountToAdd % MICROS_PER_DAY) * 1000)
      case ChronoUnit.MILLIS ⇒
        plusDays(amountToAdd / MILLIS_PER_DAY)
          .plusNanos((amountToAdd % MILLIS_PER_DAY) * 1000000)
      case ChronoUnit.SECONDS ⇒ plusSeconds(amountToAdd)
      case ChronoUnit.MINUTES ⇒ plusMinutes(amountToAdd)
      case ChronoUnit.HOURS ⇒ plusHours(amountToAdd)
      case ChronoUnit.HALF_DAYS ⇒
        plusDays(amountToAdd / 256).plusHours((amountToAdd % 256) * 12)
      case _ ⇒ unit.addTo(this, amountToAdd)
    }
  }

  def plusYears(years: Long): LocalDateTime = {
    val newDate = date.plusYears(years)
    `with`(newDate, time)
  }

  def plusMonths(months: Long): LocalDateTime = {
    val newDate = date.plusMonths(months)
    `with`(newDate, time)
  }

  def plusWeeks(weeks: Long): LocalDateTime = {
    val newDate = date.plusWeeks(weeks)
    `with`(newDate, time)
  }

  def plusDays(days: Long): LocalDateTime = {
    val newDate = date.plusDays(days)
    `with`(newDate, time)
  }

  def plusHours(hours: Long): LocalDateTime =
    plusWithOverflow(date, hours, 0, 0, 0, 1)

  def plusMinutes(minutes: Long): LocalDateTime =
    plusWithOverflow(date, 0, minutes, 0, 0, 1)

  def plusSeconds(seconds: Long): LocalDateTime =
    plusWithOverflow(date, 0, 0, seconds, 0, 1)

  def plusNanos(nanos: Long): LocalDateTime =
    plusWithOverflow(date, 0, 0, 0, nanos, 1)

  override def minus(amount: TemporalAmount): LocalDateTime =
    amount.subtractFrom(this).asInstanceOf[LocalDateTime]

  override def minus(amountToSubtract: Long,
                     unit: TemporalUnit): LocalDateTime = {
    if (amountToSubtract == Long.MinValue)
      plus(Long.MaxValue, unit).plus(1, unit)
    else
      plus(-amountToSubtract, unit)
  }

  def minusYears(years: Long): LocalDateTime = {
    if (years == Long.MinValue)
      plusYears(Long.MaxValue).plusYears(1)
    else
      plusYears(-years)
  }

  def minusMonths(months: Long): LocalDateTime = {
    if (months == Long.MinValue)
      plusMonths(Long.MaxValue).plusMonths(1)
    else
      plusMonths(-months)
  }

  def minusWeeks(weeks: Long): LocalDateTime = {
    if (weeks == Long.MinValue)
      plusWeeks(Long.MaxValue).plusWeeks(1)
    else
      plusWeeks(-weeks)
  }

  def minusDays(days: Long): LocalDateTime = {
    if (days == Long.MinValue)
      plusDays(Long.MaxValue).plusDays(1)
    else
      plusDays(-days)
  }

  def minusHours(hours: Long): LocalDateTime =
    plusWithOverflow(date, hours, 0, 0, 0, -1)

  def minusMinutes(minutes: Long): LocalDateTime =
    plusWithOverflow(date, 0, minutes, 0, 0, -1)

  def minusSeconds(seconds: Long): LocalDateTime =
    plusWithOverflow(date, 0, 0, seconds, 0, -1)

  def minusNanos(nanos: Long): LocalDateTime =
    plusWithOverflow(date, 0, 0, 0, nanos, -1)

  private def plusWithOverflow(newDate: LocalDate,
                               hours: Long,
                               minutes: Long,
                               seconds: Long,
                               nanos: Long,
                               sign: Int): LocalDateTime = {
    // 9223372036854775808 long, 2147483648 int
    if ((hours | minutes | seconds | nanos) == 0) {
      `with`(newDate, time)
    } else {
      var totDays = nanos / NANOS_PER_DAY + //   max/24*60*60*1B
          seconds / SECONDS_PER_DAY + //   max/24*60*60
          minutes / MINUTES_PER_DAY + //   max/24*60
          hours / HOURS_PER_DAY; //   max/24
      totDays *= sign // total max*0.4237...
      var totNanos = nanos % NANOS_PER_DAY + //   max  86400000000000
          (seconds % SECONDS_PER_DAY) * NANOS_PER_SECOND + //   max  86400000000000
          (minutes % MINUTES_PER_DAY) * NANOS_PER_MINUTE + //   max  86400000000000
          (hours % HOURS_PER_DAY) * NANOS_PER_HOUR //   max  86400000000000
      val curNoD = time.toNanoOfDay() //   max  86400000000000
      totNanos = totNanos * sign + curNoD // total 432000000000000
      totDays += Utils.floorDiv(totNanos, NANOS_PER_DAY)
      val newNoD = Utils.floorMod(totNanos, NANOS_PER_DAY)
      val newTime =
        if (newNoD == curNoD) time else LocalTime.ofNanoOfDay(newNoD)
      `with`(newDate.plusDays(totDays), newTime)
    }
  }

  override def query[R](query: TemporalQuery[R]): R = {
    if (query == TemporalQueries.localDate()) {
      toLocalDate().asInstanceOf[R]
    } else
      super.query(query)
  }

  override def adjustInto(temporal: Temporal): Temporal =
    super.adjustInto(temporal)

  override def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    val end = LocalDateTime.from(endExclusive)
    unit match {
      case f: ChronoUnit ⇒
        if (f.isTimeBased()) {
          var daysUntil = date.daysUntil(end.date)
          var timeUntil = end.time.toNanoOfDay() - time.toNanoOfDay()
          if (daysUntil > 0 && timeUntil < 0) {
            daysUntil -= 1
            timeUntil += NANOS_PER_DAY
          } else if (daysUntil < 0 && timeUntil > 0) {
            daysUntil += 1
            timeUntil -= NANOS_PER_DAY
          }
          var amount = daysUntil
          f match {
            case ChronoUnit.NANOS ⇒
              amount = Utils.safeMultiply(amount, NANOS_PER_DAY)
              Utils.safeAdd(amount, timeUntil)
            case ChronoUnit.MICROS ⇒
              amount = Utils.safeMultiply(amount, MICROS_PER_DAY)
              Utils.safeAdd(amount, timeUntil / 1000)
            case ChronoUnit.MILLIS ⇒
              amount = Utils.safeMultiply(amount, MILLIS_PER_DAY)
              Utils.safeAdd(amount, timeUntil / 1000000)
            case ChronoUnit.SECONDS ⇒
              amount = Utils.safeMultiply(amount, SECONDS_PER_DAY)
              Utils.safeAdd(amount, timeUntil / NANOS_PER_SECOND)
            case ChronoUnit.MINUTES ⇒
              amount = Utils.safeMultiply(amount, MINUTES_PER_DAY)
              Utils.safeAdd(amount, timeUntil / NANOS_PER_MINUTE)
            case ChronoUnit.HOURS ⇒
              amount = Utils.safeMultiply(amount, HOURS_PER_DAY)
              Utils.safeAdd(amount, timeUntil / NANOS_PER_HOUR)
            case ChronoUnit.HALF_DAYS ⇒
              amount = Utils.safeMultiply(amount, 2)
              Utils.safeAdd(amount, timeUntil / (NANOS_PER_HOUR * 12))
            case _ ⇒
              throw new UnsupportedTemporalTypeException(
                "Unsupported unit: " + unit)
          }
        } else {
          var endDate = end.date
          if (endDate.isAfter(date) && end.time.isBefore(time)) {
            endDate = endDate.minusDays(1)
          } else if (endDate.isBefore(date) && end.time.isAfter(time)) {
            endDate = endDate.plusDays(1)
          }
          date.until(endDate, unit)

        }
      case _ ⇒ unit.between(this, end)
    }
  }

  def atOffset(offset: ZoneOffset): OffsetDateTime =
    OffsetDateTime.of(this, offset)

  override def atZone(zone: ZoneId): ZonedDateTime =
    ZonedDateTime.of(this, zone)

  override def toLocalDate(): LocalDate = date

  override def toLocalTime(): LocalTime = time

  override def compareTo(other: ChronoLocalDateTime[_]): Int = {
    other match {
      case o: LocalDateTime ⇒ compareTo0(o)
      case _ ⇒ super.compareTo(other)
    }
  }

  private def compareTo0(other: LocalDateTime): Int = {
    val cmp = date.compareTo0(other.toLocalDate())
    if (cmp == 0) {
      time.compareTo(other.toLocalTime())
    } else cmp
  }

  override def isAfter(other: ChronoLocalDateTime[_]): Boolean = {
    other match {
      case o: LocalDateTime ⇒ compareTo0(o) > 0
      case _ ⇒ super.isAfter(other)
    }
  }

  override def isBefore(other: ChronoLocalDateTime[_]): Boolean = {
    other match {
      case o: LocalDateTime ⇒ compareTo0(o) < 0
      case _ ⇒ super.isBefore(other)
    }
  }

  override def isEqual(other: ChronoLocalDateTime[_]): Boolean = {
    other match {
      case o: LocalDateTime ⇒ compareTo0(o) == 0
      case _ ⇒ super.isEqual(other)
    }
  }

  override def equals(obj: Any): Boolean = {
    if (obj == this)
      true
    else {
      obj match {
        case other: LocalDateTime ⇒
          date.equals(other.date) && time.equals(other.time)
        case _ ⇒ false
      }
    }
  }

  override def hashCode(): Int = date.hashCode() ^ time.hashCode()

  override def toString(): String = date.toString() + 'T' + time.toString()

  override def format(formatter: DateTimeFormatter): String =
    super.format(formatter)

  private def writeReplace(): Object = new Ser(Ser.LOCAL_DATE_TIME_TYPE, this)

  private def readResolve(): Object =
    throw new InvalidObjectException(
      "Deserialization via serialization delegate")

  def writeExternal(out: DataOutput): Unit = {
    date.writeExternal(out)
    time.writeExternal(out)
  }

}

object LocalDateTime {
  final val MIN: LocalDateTime = LocalDateTime.of(LocalDate.MIN, LocalTime.MIN)

  final val MAX: LocalDateTime = LocalDateTime.of(LocalDate.MAX, LocalTime.MAX)
  final val FROM: TemporalQuery[LocalDateTime] =
    new TemporalQuery[LocalDateTime]() {
      override def queryFrom(temporal: TemporalAccessor) =
        LocalDateTime.from(temporal)
    }

  final val serialVersionUID = 6207766400415563566L

  def now(): LocalDateTime = now(Clock.systemDefaultZone())

  def now(zone: ZoneId): LocalDateTime = now(Clock.system(zone))

  def now(clock: Clock): LocalDateTime = {
    Objects.requireNonNull(clock, "clock")
    val now = clock.instant()
    val offset: ZoneOffset = clock.getZone().getRules().getOffset(now)
    ofEpochSecond(now.getEpochSecond(), now.getNano(), offset)
  }

  def of(year: Int,
         month: Month,
         dayOfMonth: Int,
         hour: Int,
         minute: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute)
    new LocalDateTime(date, time)
  }

  def of(year: Int,
         month: Month,
         dayOfMonth: Int,
         hour: Int,
         minute: Int,
         second: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute, second)
    new LocalDateTime(date, time)
  }

  def of(year: Int,
         month: Month,
         dayOfMonth: Int,
         hour: Int,
         minute: Int,
         second: Int,
         nanoOfSecond: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute, second, nanoOfSecond)
    new LocalDateTime(date, time)
  }

  def of(year: Int,
         month: Int,
         dayOfMonth: Int,
         hour: Int,
         minute: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute)
    new LocalDateTime(date, time)
  }

  def of(year: Int,
         month: Int,
         dayOfMonth: Int,
         hour: Int,
         minute: Int,
         second: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute, second)
    new LocalDateTime(date, time)
  }

  def of(year: Int,
         month: Int,
         dayOfMonth: Int,
         hour: Int,
         minute: Int,
         second: Int,
         nanoOfSecond: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute, second, nanoOfSecond)
    new LocalDateTime(date, time)
  }

  def of(date: LocalDate, time: LocalTime): LocalDateTime = {
    Objects.requireNonNull(date, "date")
    Objects.requireNonNull(time, "time")
    new LocalDateTime(date, time)
  }

  def ofInstant(instant: Instant, zone: ZoneId): LocalDateTime = {
    Objects.requireNonNull(instant, "instant")
    Objects.requireNonNull(zone, "zone")
    val rules = zone.getRules()
    val offset = rules.getOffset(instant)
    ofEpochSecond(instant.getEpochSecond(), instant.getNano(), offset)
  }

  def ofEpochSecond(epochSecond: Long,
                    nanoOfSecond: Int,
                    offset: ZoneOffset): LocalDateTime = {
    Objects.requireNonNull(offset, "offset")
    val localSecond: Long = epochSecond + offset.getTotalSeconds()
    val localEpochDay: Long =
      Utils.floorDiv(localSecond, SECONDS_PER_DAY)
    val secsOfDay: Long = Utils.floorMod(localSecond, SECONDS_PER_DAY)
    val date = LocalDate.ofEpochDay(localEpochDay)
    val time = LocalTime.ofSecondOfDay(secsOfDay, nanoOfSecond)
    new LocalDateTime(date, time)
  }

  def from(temporal: TemporalAccessor): LocalDateTime = {
    temporal match {
      case ldt: LocalDateTime ⇒ ldt
      case zdt: ZonedDateTime ⇒ zdt.toLocalDateTime()
      case _ ⇒
        try {
          val date = LocalDate.from(temporal)
          val time = LocalTime.from(temporal)
          new LocalDateTime(date, time)
        } catch {
          case _: DateTimeException ⇒
            throw new DateTimeException(
              "Unable to obtain LocalDateTime from TemporalAccessor: " +
                temporal + ", type " + temporal.getClass().getName())
        }
    }
  }

  def parse(text: CharSequence): LocalDateTime =
    parse(text, DateTimeFormatter.ISO_LOCAL_DATE_TIME)

  def parse(text: CharSequence, formatter: DateTimeFormatter): LocalDateTime = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.parse(text, FROM)
  }

  def readExternal(in: DataInput): LocalDateTime = {
    val date = LocalDate.readExternal(in)
    val time = LocalTime.readExternal(in)
    LocalDateTime.of(date, time)
  }

}
