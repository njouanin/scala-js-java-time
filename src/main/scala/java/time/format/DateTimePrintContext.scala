/*
 *********************************************************************************
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <nico@beerfactory.org> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Nicolas JOUANIN
 *********************************************************************************
 */
package java.time.format

import java.time.DateTimeException
import java.time.temporal.{TemporalAccessor, TemporalField, TemporalQuery}
import java.util.Locale

final class DateTimePrintContext(temporal: TemporalAccessor,
                                 locale: Locale,
                                 symbols: DecimalStyle) {

  var optional: Int = _
  def this(temporal: TemporalAccessor, formatter: DateTimeFormatter) =
    this(temporal, formatter.getLocale, formatter.getDecimalStyle)

  def startOptional(): Unit = optional += 1

  def endOptional(): Unit = optional -= 1

  def getValue[R](query: TemporalQuery[R]): R = {
    val result: R = temporal.query(query)
    if (result == null && optional == 0) {
      throw new DateTimeException(
        "Unable to extract value: " + temporal.getClass)
    }
    result
  }

  def getValue(field: TemporalField): Long = temporal.getLong(field)

  def getTemporal(): TemporalAccessor = temporal

  def getSymbols(): DecimalStyle = symbols

}
