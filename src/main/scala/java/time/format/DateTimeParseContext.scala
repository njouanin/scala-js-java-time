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
import java.time.chrono.Chronology
import java.time.temporal.TemporalField
import java.util
import java.util.Locale

final class DateTimeParseContext(
    private[DateTimeParseContext] val locale: Locale,
    private[DateTimeParseContext] val symbols: DecimalStyle,
    private[DateTimeParseContext] val chronology: Chronology) {

  private[DateTimeParseContext] var overrideZone: Option[ZoneId] = None
  private[DateTimeParseContext] var parsed = new util.ArrayList[Parsed]()
  private[DateTimeParseContext] var caseSensitive: Boolean = true
  private[DateTimeParseContext] var strict: Boolean = true

  parsed.add(new Parsed())

  def this(formatter: DateTimeFormatter) = {
    this(formatter.getLocale,
         formatter.getDecimalStyle,
         formatter.getChronology)
    overrideZone = Some(formatter.getZone)
  }

  def this(other: DateTimeParseContext) = {
    this(other.locale, other.symbols, other.chronology)
    overrideZone = other.overrideZone
    caseSensitive = other.caseSensitive
    strict = other.strict
  }

  def copy(): DateTimeParseContext = new DateTimeParseContext(this)

  def setStrict(strict: Boolean): Unit = this.strict = strict
  def setCaseSensitive(caseSensitive: Boolean): Unit =
    this.caseSensitive = caseSensitive

  private def currentParsed(): Parsed = parsed.get(parsed.size() - 1)
  def isCaseSensitive(): Boolean = caseSensitive
  def isStrict(): Boolean = strict

  def charEquals(ch1: Char, ch2: Char): Boolean = {
    if (isCaseSensitive())
      ch1 == ch2
    else
      DateTimeParseContext.charEqualsIgnoreCase(ch1, ch2)
  }

  def startOptional(): Unit = {
    parsed.add(currentParsed().copy());
  }

  def endOptional(successful: Boolean): Unit = {
    if (successful) {
      parsed.remove(parsed.size() - 2)
    } else {
      parsed.remove(parsed.size() - 1)
    }
  }

  def getParsed(field: TemporalField): Long =
    currentParsed().fieldValues.get(field)
}

object DateTimeParseContext {
  def charEqualsIgnoreCase(c1: Char, c2: Char): Boolean = {
    c1 == c2 ||
    Character.toUpperCase(c1) == Character.toUpperCase(c2) ||
    Character.toLowerCase(c1) == Character.toLowerCase(c2)
  }

}
