/*
 *********************************************************************************
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <nico@beerfactory.org> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Nicolas JOUANIN
 *********************************************************************************
 */
package java.time.format

import java.time.ZoneId
import java.time.chrono.{Chronology, IsoChronology}
import java.time.format.DateTimeFormatterBuilder.CompositePrinterParser
import java.time.temporal.{TemporalAccessor, TemporalField}
import java.util.Locale
import java.time.temporal.ChronoField._

class DateTimeFormatter private (
    private[this] val printerParser: CompositePrinterParser,
    private[this] val locale: Locale,
    private[this] val decimalStyle: DecimalStyle,
    private[this] val resolverStyle: ResolverStyle,
    private[this] val resolverFields: java.util.Set[TemporalField],
    private[this] val chrono: Chronology,
    private[this] val zone: ZoneId) {

  def getDecimalStyle(): DecimalStyle = decimalStyle
  def getLocale(): Locale = locale
  def getZone(): ZoneId = zone
  def getChronology(): Chronology = chrono

  def withLocale(locale: Locale): DateTimeFormatter = {
    if (this.locale.equals(locale))
      this
    else
      new DateTimeFormatter(printerParser,
                            locale,
                            decimalStyle,
                            resolverStyle,
                            resolverFields,
                            chrono,
                            zone)
  }

  def format(temporal: TemporalAccessor): String = ???

  def formatTo(temportal: TemporalAccessor, appendable: Appendable): Unit = ???

}

object DateTimeFormatter {
  val ISO_INSTANT = new DateTimeFormatterBuilder()
    .parseCaseInsensitive()
    .appendInstant()
    .toFormatter(ResolverStyle.STRICT)

  val ISO_LOCAL_DATE = new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
    .appendLiteral('-')
    .appendValue(MONTH_OF_YEAR, 2)
    .appendLiteral('-')
    .appendValue(DAY_OF_MONTH, 2)
    .toFormatter(ResolverStyle.STRICT)
    .withChronology(IsoChronology.INSTANCE)

  val ISO_LOCAL_DATE_TIME = new DateTimeFormatterBuilder()
    .parseCaseInsensitive()
    .append(ISO_LOCAL_DATE)
    .appendLiteral('T')
    .append(ISO_LOCAL_TIME)
    .toFormatter(ResolverStyle.STRICT)
    .withChronology(IsoChronology.INSTANCE)

}
