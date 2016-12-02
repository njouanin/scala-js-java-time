/*
 *********************************************************************************
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <nico@beerfactory.org> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Nicolas JOUANIN
 *********************************************************************************
 */
package java.time.format

import java.text.ParsePosition
import java.time.ZoneId
import java.time.chrono.{Chronology, IsoChronology}
import java.time.format.DateTimeFormatterBuilder.CompositePrinterParser
import java.time.temporal.{TemporalAccessor, TemporalField, TemporalQuery}
import java.util.{Locale, Objects}
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

  def parse(text: CharSequence): TemporalAccessor = {
    Objects.requireNonNull(text, "text")
    try {
      parseToBuilder(text, null).resolve(resolverStyle, resolverFields)
    } catch {
      case ex: DateTimeParseException ⇒ throw ex
      case ex: RuntimeException ⇒ throw createError(text, ex)
    }
  }

  def parse(text: CharSequence, position: ParsePosition): TemporalAccessor = {
    Objects.requireNonNull(text, "text")
    Objects.requireNonNull(position, "position")
    try {
      parseToBuilder(text, position).resolve(resolverStyle, resolverFields)
    } catch {
      case ex: DateTimeParseException ⇒ throw ex
      case ex: IndexOutOfBoundsException ⇒ throw ex
      case ex: RuntimeException ⇒ throw createError(text, ex)
    }
  }

  def parse[T](text: CharSequence, `type`: TemporalQuery[T]): T = {
    Objects.requireNonNull(text, "text")
    Objects.requireNonNull(`type`, "type")
    try {
      val builder =
        parseToBuilder(text, null).resolve(resolverStyle, resolverFields)
      builder.build(`type`)
    } catch {
      case ex: DateTimeParseException ⇒ throw ex
      case ex: RuntimeException ⇒ throw createError(text, ex)
    }
  }

  private def createError(text: CharSequence,
                          ex: RuntimeException): DateTimeParseException = {
    var abbr = ""
    if (text.length() > 64) {
      abbr = text.subSequence(0, 64).toString() + "..."
    } else {
      abbr = text.toString()
    }
    new DateTimeParseException(
      s"Text '$abbr' could not be parsed: " + ex.getMessage(),
      text,
      0,
      ex)
  }

  private def parseToBuilder(text: CharSequence,
                             position: ParsePosition): DateTimeBuilder = {
    val pos = if (position != null) position else new ParsePosition(0)
    val result = parseUnresolved0(text, pos)
    if (result == null || pos.getErrorIndex() >= 0 || (position == null && pos
          .getIndex() < text.length())) {
      val abbr =
        if (text.length() > 64)
          text.subSequence(0, 64).toString() + "..."
        else
          text.toString()

      if (pos.getErrorIndex() >= 0) {
        throw new DateTimeParseException(
          "Text '" + abbr + "' could not be parsed at index " +
            pos.getErrorIndex(),
          text,
          pos.getErrorIndex())
      } else {
        throw new DateTimeParseException(
          "Text '" + abbr + "' could not be parsed, unparsed text found at index " +
            pos.getIndex(),
          text,
          pos.getIndex())
      }
    }
    result.toBuilder()
  }

  def parseUnresolved(text: CharSequence,
                      position: ParsePosition): TemporalAccessor =
    parseUnresolved0(text, position)

  private def parseUnresolved0(text: CharSequence,
                               position: ParsePosition): Parsed = {
    Objects.requireNonNull(text, "text")
    Objects.requireNonNull(position, "position")
    val context = new DateTimeParseContext(this)
    var pos = position.getIndex()
    pos = printerParser.parse(context, text, pos)
    if (pos < 0) {
      position.setErrorIndex(~pos)
      null
    } else {
      position.setIndex(pos)
      context.toParsed()
    }
  }

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

  val ISO_LOCAL_TIME = new DateTimeFormatterBuilder()
    .appendValue(HOUR_OF_DAY, 2)
    .appendLiteral(':')
    .appendValue(MINUTE_OF_HOUR, 2)
    .optionalStart()
    .appendLiteral(':')
    .appendValue(SECOND_OF_MINUTE, 2)
    .optionalStart()
    .appendFraction(NANO_OF_SECOND, 0, 9, true)
    .toFormatter(ResolverStyle.STRICT)

}
