/*
 *********************************************************************************
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <nico@beerfactory.org> wrote this file.  As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return.   Nicolas JOUANIN
 *********************************************************************************
 */
package java.time.format

import java.math.BigInteger
import java.text.{DateFormat, SimpleDateFormat}
import java.time.{DateTimeException, LocalDateTime, Utils, ZoneOffset}
import java.time.chrono.Chronology
import java.time.format.DateTimeFormatterBuilder._
import java.time.format.FormatStyle.FormatStyle
import java.time.temporal.{ChronoField, TemporalField}
import java.util
import java.util.{Locale, Objects}
import java.time.temporal.ChronoField._

final class DateTimeFormatterBuilder private (
    private[this] var parent: Option[DateTimeFormatterBuilder],
    private[this] var optional: Boolean) {

  private val active: DateTimeFormatterBuilder = this
  private var padNextWidth: Int = _
  private var padNextChar: Char = _
  private var valueParserIndex: Int = -1
  private val printerParsers =
    new util.ArrayList[DateTimeFormatterBuilder.DateTimePrinterParser]();

  def this() = this(None, false)

  def parseCaseSensitive(): DateTimeFormatterBuilder = {
    appendInternal(SettingsParser.SENSITIVE)
    this
  }

  def parseCaseInsensitive(): DateTimeFormatterBuilder = {
    appendInternal(SettingsParser.INSENSITIVE)
    this
  }

  def appendInstant(): DateTimeFormatterBuilder = {
    appendInternal(new InstantPrinterParser(-2))
    this;
  }

  def appendValue(field: TemporalField): DateTimeFormatterBuilder = {
    Objects.requireNonNull(field, "field")
    appendValue(new NumberPrinterParser(field, 1, 19, SignStyle.NORMAL))
  }

  def appendValue(field: TemporalField, width: Int): DateTimeFormatterBuilder = {
    Objects.requireNonNull(field, "field")
    if (width < 1 || width > 19) {
      throw new IllegalArgumentException(
        s"The width must be from 1 to 19 inclusive but was $width")
    }
    val pp =
      new NumberPrinterParser(field, width, width, SignStyle.NOT_NEGATIVE)
    appendValue(pp)
    this
  }

  def appendValue(field: TemporalField,
                  minWidth: Int,
                  maxWidth: Int,
                  signStyle: SignStyle): DateTimeFormatterBuilder = {
    if (minWidth == maxWidth && signStyle == SignStyle.NOT_NEGATIVE) {
      appendValue(field, maxWidth)
    } else {
      Objects.requireNonNull(field, "field")
      Objects.requireNonNull(signStyle, "signStyle")
      if (minWidth < 1 || minWidth > 19) {
        throw new IllegalArgumentException(
          s"The minimum width must be from 1 to 19 inclusive but was $minWidth")
      }
      if (maxWidth < 1 || maxWidth > 19) {
        throw new IllegalArgumentException(
          s"The maximum width must be from 1 to 19 inclusive but was $maxWidth")
      }
      if (maxWidth < minWidth) {
        throw new IllegalArgumentException(
          s"The maximum width must exceed or equal the minimum width but $maxWidth < $minWidth")
      }
      val pp = new NumberPrinterParser(field, minWidth, maxWidth, signStyle)
      appendValue(pp)
      this
    }
  }

  private def appendValue(pp: NumberPrinterParser): DateTimeFormatterBuilder = {
    if (active.valueParserIndex >= 0 &&
        active.printerParsers
          .get(active.valueParserIndex)
          .isInstanceOf[NumberPrinterParser]) {
      val activeValueParser = active.valueParserIndex

      // adjacent parsing mode, update setting in previous parsers
      var basePP = active.printerParsers
        .get(activeValueParser)
        .asInstanceOf[NumberPrinterParser]
      if (pp.minWidth == pp.maxWidth && pp.signStyle == SignStyle.NOT_NEGATIVE) {
        // Append the width to the subsequentWidth of the active parser
        basePP = basePP.withSubsequentWidth(pp.maxWidth)
        // Append the new parser as a fixed width
        appendInternal(pp.withFixedWidth())
        // Retain the previous active parser
        active.valueParserIndex = activeValueParser
      } else {
        // Modify the active parser to be fixed width
        basePP = basePP.withFixedWidth()
        // The new parser becomes the mew active parser
        active.valueParserIndex = appendInternal(pp)
      }
      // Replace the modified parser with the updated one
      active.printerParsers.set(activeValueParser, basePP)
    } else {
      // The new Parser becomes the active parser
      active.valueParserIndex = appendInternal(pp)
    }
    this
  }

  private def appendInternal(pp: DateTimePrinterParser): Integer = {
    Objects.requireNonNull(pp, "pp")
    active.printerParsers.add(if (active.padNextWidth > 0) {
      active.padNextWidth = 0
      active.padNextChar = 0
      new DateTimeFormatterBuilder.PadPrinterParserDecorator(
        pp,
        active.padNextWidth,
        active.padNextChar)
    } else pp)
    active.valueParserIndex = -1
    active.printerParsers.size() - 1
  }

  def append(formatter: DateTimeFormatter): DateTimeFormatterBuilder = {
    Objects.requireNonNull(formatter, "formatter")
    appendInternal(formatter.toPrinterParser(false))
    this
  }

  def appendLiteral(literal: Char): DateTimeFormatterBuilder = {
    appendInternal(new CharLiteralPrinterParser(literal))
    this
  }

  def appendLiteral(literal: String): DateTimeFormatterBuilder = {
    Objects.requireNonNull(literal, "literal")
    if (literal.length() > 0) {
      if (literal.length() == 1) {
        appendInternal(new CharLiteralPrinterParser(literal.charAt(0)))
      } else {
        appendInternal(new StringLiteralPrinterParser(literal))
      }
    }
    this
  }
}

object DateTimeFormatterBuilder {
  def getLocalizedDateTimePattern(dateStyle: FormatStyle,
                                  timeStyle: FormatStyle,
                                  chrono: Chronology,
                                  locale: Locale): String = {

    Objects.requireNonNull(locale, "locale");
    Objects.requireNonNull(chrono, "chrono");

    val dateFormat = (dateStyle, timeStyle) match {
      case (null, null) ⇒
        throw new IllegalArgumentException(
          "Either dateStyle or timeStyle must be non-null");
      case (dStyle: FormatStyle, null) ⇒
        DateFormat.getDateInstance(dStyle.id, locale);
      case (null, tStyle: FormatStyle) ⇒
        DateFormat.getTimeInstance(tStyle.id, locale);
      case (dStyle: FormatStyle, tStyle: FormatStyle) ⇒
        DateFormat.getDateTimeInstance(dStyle.id, tStyle.id, locale);
    }
    dateFormat match {
      case sdf: SimpleDateFormat ⇒ sdf.toPattern
      case _ ⇒
        throw new IllegalArgumentException("Unable to determine pattern")
    }
  }

  class PadPrinterParserDecorator(
      private[this] val printerParser: DateTimePrinterParser,
      private[this] val padWidth: Int,
      private[this] val padChar: Char)
      extends DateTimePrinterParser {

    override def print(context: DateTimePrintContext,
                       buf: StringBuilder): Boolean = {
      val preLen = buf.length()
      if (!printerParser.print(context, buf)) {
        false
      } else {
        val len = buf.length() - preLen
        if (len > padWidth) {
          throw new DateTimeException(
            "Cannot print as output of " + len + " characters exceeds pad width of " + padWidth)
        }
        for (_ <- 0 to padWidth - len) {
          buf.insert(preLen, padChar)
        }
        true
      }
    }

    override def parse(context: DateTimeParseContext,
                       text: CharSequence,
                       position: Int): Int = {
      val strict = context.isStrict()
      val caseSensitive = context.isCaseSensitive()

      if (position > text.length()) {
        throw new IndexOutOfBoundsException()
      } else {
        if (position == text.length())
          ~position; // no more characters in the string
        else {
          if (strict && position + padWidth > text.length()) {
            ~position // not enough characters in the string to meet the parse width
          } else {
            val endPos =
              if (position + padWidth > text.length()) text.length()
              else position + padWidth
            var pos = position
            while (pos < endPos &&
                   (if (caseSensitive) text.charAt(pos) == padChar
                    else context.charEquals(text.charAt(pos), padChar))) {
              pos += 1
            }
            val t = text.subSequence(0, endPos)
            val resultPos = printerParser.parse(context, t, pos)
            if (resultPos != endPos && strict) {
              ~(position + pos) // parse of decorated field didn't parse to the end
            } else
              resultPos
          }
        }
      }
    }

    override def toString(): String = {
      val sep = if (padChar == ' ') ")" else ",'"
      "Pad(" + printerParser + "," + padWidth + sep + padChar + "')"
    }
  }

  object SettingsParser extends Enumeration with DateTimePrinterParser {
    type SettingsParser = Value
    val SENSITIVE, INSENSITIVE, STRICT, LENIENT = new Value
    with DateTimePrinterParser {
      override def print(context: DateTimePrintContext,
                         buf: StringBuilder): Boolean = true
      override def parse(context: DateTimeParseContext,
                         text: CharSequence,
                         position: Int): Int = {
        id match {
          case 0 => context.setCaseSensitive(true)
          case 1 => context.setCaseSensitive(false)
          case 2 => context.setStrict(true)
          case 3 => context.setStrict(false)
        }
        position
      }

      override def toString(): String = {
        // using ordinals to avoid javac synthetic inner class
        id match {
          case 0 => "ParseCaseSensitive(true)"
          case 1 ⇒ "ParseCaseSensitive(false)"
          case 2 ⇒ "ParseStrict(true)"
          case 3 ⇒ "ParseStrict(false)"
        }
        throw new IllegalStateException("Unreachable");
      }
    }
  }

  trait DateTimePrinterParser {
    def print(context: DateTimePrintContext, buf: StringBuilder): Boolean
    def parse(context: DateTimeParseContext,
              text: CharSequence,
              position: Int): Int
  }

  final class CompositePrinterParser(
      printerParsers: Array[DateTimePrinterParser],
      optional: Boolean)
      extends DateTimePrinterParser {

    def this(printerParsers: java.util.List[DateTimePrinterParser],
             optional: Boolean) =
      this(printerParsers.toArray(
             new Array[DateTimePrinterParser](printerParsers.size())),
           optional)

    override def print(context: DateTimePrintContext,
                       buf: StringBuilder): Boolean = {
      val length = buf.length()
      if (optional) {
        context.startOptional()
      }

      if (printerParsers == null || !printerParsers.forall(
            pp ⇒ pp.print(context, buf)))
        buf.setLength(length)

      if (optional) {
        context.endOptional()
      }
      true
    }

    override def parse(context: DateTimeParseContext,
                       text: CharSequence,
                       position: Int): Int = {
      if (optional) {
        context.startOptional()

      }
      var pos = position
      for (pp ← printerParsers if pos >= 0)
        pos = pp.parse(context, text, pos)

      if (optional) {
        context.endOptional(pos >= 0)
      }
      pos
    }

    override def toString: String = {
      val buf = new StringBuilder()
      if (printerParsers != null) {
        val buf = new StringBuilder()
        buf.append(if (optional) "[" else "(")
        printerParsers.foreach(pp ⇒ buf.append(pp))
        buf.append(if (optional) "]" else ")")
      }
      buf.toString()
    }
  }

  object InstantPrinterParser {
    val SECONDS_PER_10000_YEARS = 146097L * 25L * 86400L
    val SECONDS_0000_TO_1970 = ((146097L * 5L) - (30L * 365L + 7L)) * 86400L

  }

  class InstantPrinterParser(fractionalDigits: Int)
      extends DateTimePrinterParser {
    override def print(context: DateTimePrintContext,
                       buf: StringBuilder): Boolean = {
      // use INSTANT_SECONDS, thus this code is not bound by Instant.MAX
      val inSecs = context.getValue(ChronoField.INSTANT_SECONDS)
      val inNanos =
        if (context.getTemporal().isSupported(ChronoField.NANO_OF_SECOND)) {
          context.getTemporal().getLong(ChronoField.NANO_OF_SECOND)
        } else
          0L

      if (inSecs == null) {
        return false
      }
      val inSec = inSecs
      var inNano = ChronoField.NANO_OF_SECOND.checkValidIntValue(inNanos)
      if (inSec >= -InstantPrinterParser.SECONDS_0000_TO_1970) {
        // current era
        val zeroSecs = inSec - InstantPrinterParser.SECONDS_PER_10000_YEARS + InstantPrinterParser.SECONDS_0000_TO_1970
        val hi = Utils.floorDiv(
            zeroSecs,
            InstantPrinterParser.SECONDS_PER_10000_YEARS) + 1
        val lo = Utils
          .floorMod(zeroSecs, InstantPrinterParser.SECONDS_PER_10000_YEARS)
        val ldt: LocalDateTime = LocalDateTime.ofEpochSecond(
          lo - InstantPrinterParser.SECONDS_0000_TO_1970,
          0,
          ZoneOffset.UTC)
        if (hi > 0) {
          buf.append('+').append(hi)
        }
        buf.append(ldt)
        if (ldt.getSecond() == 0) {
          buf.append(":00")
        }
      } else {
        // before current era
        val zeroSecs = inSec + InstantPrinterParser.SECONDS_0000_TO_1970
        val hi: Long = zeroSecs / InstantPrinterParser.SECONDS_PER_10000_YEARS
        val lo = zeroSecs % InstantPrinterParser.SECONDS_PER_10000_YEARS
        val ldt: LocalDateTime = LocalDateTime.ofEpochSecond(
          lo - InstantPrinterParser.SECONDS_0000_TO_1970,
          0,
          ZoneOffset.UTC)
        val pos = buf.length()
        buf.append(ldt)
        if (ldt.getSecond() == 0) {
          buf.append(":00")
        }
        if (hi < 0) {
          if (ldt.getYear() == -10000) {
            buf.replace(pos, pos + 2, (hi - 1).toString)
          } else if (lo == 0) {
            buf.insert(pos, hi)
          } else {
            buf.insert(pos + 1, Math.abs(hi))
          }
        }
      }
      //fraction
      if (fractionalDigits == -2) {
        if (inNano != 0) {
          buf.append('.')
          if (inNano % 1000000 == 0) {
            buf.append(
              Integer.toString((inNano / 1000000) + 1000).substring(1))
          } else if (inNano % 1000 == 0) {
            buf.append(
              Integer.toString((inNano / 1000) + 1000000).substring(1))
          } else {
            buf.append(Integer.toString((inNano) + 1000000000).substring(1))
          }
        }
      } else if (fractionalDigits > 0 || (fractionalDigits == -1 && inNano > 0)) {
        buf.append('.')
        var div = 100000000
        var i = 0
        while ((fractionalDigits == -1 && inNano > 0) || i < fractionalDigits) {
          var digit: Int = inNano / div
          buf.append(digit + '0')
          inNano = inNano - (digit * div)
          div = div / 10
          i += 1
        }
      }
      buf.append('Z')
      true
    }

    override def parse(context: DateTimeParseContext,
                       text: CharSequence,
                       position: Int): Int = {
      // new context to avoid overwriting fields like year/month/day
      val newContext: DateTimeParseContext = context.copy()
      val minDigits = if (fractionalDigits < 0) 0 else fractionalDigits
      val maxDigits = if (fractionalDigits < 0) 9 else fractionalDigits
      val parser: CompositePrinterParser = new DateTimeFormatterBuilder()
        .append(DateTimeFormatter.ISO_LOCAL_DATE)
        .appendLiteral('T')
        .appendValue(HOUR_OF_DAY, 2)
        .appendLiteral(':')
        .appendValue(MINUTE_OF_HOUR, 2)
        .appendLiteral(':')
        .appendValue(SECOND_OF_MINUTE, 2)
        .appendFraction(NANO_OF_SECOND, minDigits, maxDigits, true)
        .appendLiteral('Z')
        .toFormatter()
        .toPrinterParser(false)
      val pos = parser.parse(newContext, text, position)
      if (pos < 0) {
        pos
      } else {
        // parser restricts most fields to 2 digits, so definitely int
        // correctly parsed nano is also guaranteed to be valid
        val yearParsed = newContext.getParsed(YEAR)
        val month = newContext.getParsed(MONTH_OF_YEAR).intValue()
        val day = newContext.getParsed(DAY_OF_MONTH).intValue()
        var hour = newContext.getParsed(HOUR_OF_DAY).intValue()
        val min = newContext.getParsed(MINUTE_OF_HOUR).intValue()
        val secVal = newContext.getParsed(SECOND_OF_MINUTE)
        val nanoVal = newContext.getParsed(NANO_OF_SECOND)
        var sec = if (secVal != null) secVal.intValue() else 0
        val nano = if (nanoVal != null) nanoVal.intValue() else 0
        val year = (yearParsed % 10000).toInt
        var days = 0
        if (hour == 24 && min == 0 && sec == 0 && nano == 0) {
          hour = 0
          days = 1
        } else if (hour == 23 && min == 59 && sec == 60) {
          context.setParsedLeapSecond()
          sec = 59
        }
        try {
          val ldt =
            LocalDateTime
              .of(year, month, day, hour, min, sec, 0)
              .plusDays(days)
          var instantSecs = ldt.toEpochSecond(ZoneOffset.UTC)
          instantSecs += Utils.safeMultiply(
            yearParsed / 10000L,
            InstantPrinterParser.SECONDS_PER_10000_YEARS)
          var successPos = pos
          successPos = context.setParsedField(ChronoField.INSTANT_SECONDS,
                                              instantSecs,
                                              position,
                                              successPos)
          context.setParsedField(ChronoField.NANO_OF_SECOND,
                                 nano,
                                 position,
                                 successPos)
        } catch {
          case _: RuntimeException ⇒ ~position
        }

      }
    }

    override def toString(): String = "Instant()"
  }

  class NumberPrinterParser(protected[format] val field: TemporalField,
                            protected[format] val minWidth: Int,
                            protected[format] val maxWidth: Int,
                            protected[format] val signStyle: SignStyle,
                            protected[format] val subsequentWidth: Int = 0)
      extends DateTimePrinterParser {

    def withFixedWidth(): NumberPrinterParser = {
      if (subsequentWidth == -1) {
        this
      } else
        new NumberPrinterParser(field, minWidth, maxWidth, signStyle, -1)
    }

    def withSubsequentWidth(subsequentWidth: Int): NumberPrinterParser =
      new NumberPrinterParser(field,
                              minWidth,
                              maxWidth,
                              signStyle,
                              this.subsequentWidth + subsequentWidth)

    override def print(context: DateTimePrintContext,
                       buf: StringBuilder): Boolean = {
      val valueLong = context.getValue(field)
      if (valueLong == null) {
        false
      } else {
        val value = getValue(context, valueLong)
        val symbols = context.getSymbols()
        var str =
          if (value == Long.MinValue) "9223372036854775808"
          else Math.abs(value).toString
        if (str.length() > maxWidth) {
          throw new DateTimeException(
            s"Field $field cannot be printed as the value $value  exceeds the maximum print width of $maxWidth")
        }
        str = symbols.convertNumberToI18N(str)

        if (value >= 0) {
          signStyle match {
            case SignStyle.EXCEEDS_PAD ⇒
              if (minWidth < 19 && value >= NumberPrinterParser.EXCEED_POINTS(
                    minWidth))
                buf.append(symbols.getPositiveSign())
            case SignStyle.ALWAYS ⇒ buf.append(symbols.getPositiveSign())
          }
        } else {
          signStyle match {
            case SignStyle.NORMAL | SignStyle.EXCEEDS_PAD | SignStyle.ALWAYS ⇒
              buf.append(symbols.getNegativeSign())
            case SignStyle.NOT_NEGATIVE =>
              throw new DateTimeException(
                "Field $field cannot be printed as the value $value cannot be negative according to the SignStyle")
          }
        }
        for (i <- 0 to (minWidth - str.length())) {
          buf.append(symbols.getZeroDigit())
        }
        buf.append(str)
        true
      }
    }

    def getValue(context: DateTimePrintContext, value: Long): Long = value

    def isFixedWidth(context: DateTimeParseContext): Boolean =
      subsequentWidth == -1 ||
        (subsequentWidth > 0 && minWidth == maxWidth && signStyle == SignStyle.NOT_NEGATIVE)

    override def parse(context: DateTimeParseContext,
                       text: CharSequence,
                       p: Int): Int = {
      var position = p
      val length = text.length()
      if (position == length) {
        return ~position
      }
      val sign = text.charAt(position); // IOOBE if invalid position
      var negative = false
      var positive = false
      if (sign == context.getSymbols().getPositiveSign()) {
        if (signStyle.parse(true, context.isStrict(), minWidth == maxWidth) == false) {
          return ~position
        }
        positive = true
        position += 1
      } else if (sign == context.getSymbols().getNegativeSign()) {
        if (signStyle.parse(false, context.isStrict(), minWidth == maxWidth) == false) {
          return ~position
        }
        negative = true
        position += 1
      } else {
        if (signStyle == SignStyle.ALWAYS && context.isStrict()) {
          return ~position
        }
      }
      val effMinWidth =
        if (context.isStrict() || isFixedWidth(context)) minWidth else 1
      val minEndPos = position + effMinWidth
      if (minEndPos > length) {
        return ~position
      }
      var effMaxWidth = (if (context.isStrict() || isFixedWidth(context))
                           maxWidth
                         else 9) + Math.max(subsequentWidth, 0)
      var total = 0;
      var totalBig: BigInteger = null
      var pos = position
      var stop = false
      for (pass <- 0 to 2 if !stop) {
        val maxEndPos = Math.min(pos + effMaxWidth, length)
        while (pos < maxEndPos && !stop) {
          pos += 1
          val ch = text.charAt(pos)
          val digit = context.getSymbols().convertToDigit(ch)
          if (digit < 0) {
            pos -= 1
            if (pos < minEndPos) {
              return ~position
            }
            stop = true
          }
          if ((pos - position) > 18) {
            if (totalBig == null) {
              totalBig = BigInteger.valueOf(total)
            }
            totalBig =
              totalBig.multiply(BigInteger.TEN).add(BigInteger.valueOf(digit))
          } else {
            total = total * 10 + digit
          }
        }
        if (subsequentWidth > 0 && pass == 0) {
          // re-parse now we know the correct width
          val parseLen = pos - position
          effMaxWidth = Math.max(effMinWidth, parseLen - subsequentWidth)
          pos = position
          total = 0
          totalBig = null
        } else {
          stop = true
        }
      }
      if (negative) {
        if (totalBig != null) {
          if (totalBig.equals(BigInteger.ZERO) && context.isStrict()) {
            return ~(position - 1)
          }
          totalBig = totalBig.negate()
        } else {
          if (total == 0 && context.isStrict()) {
            return ~(position - 1)
          }
          total = -total
        }
      } else if (signStyle == SignStyle.EXCEEDS_PAD && context.isStrict()) {
        val parseLen = pos - position
        if (positive) {
          if (parseLen <= minWidth) {
            return ~(position - 1)
          }
        } else {
          if (parseLen > minWidth) {
            return ~position
          }
        }
      }
      if (totalBig != null) {
        if (totalBig.bitLength() > 63) {
          // overflow, parse 1 less digit
          totalBig = totalBig.divide(BigInteger.TEN)
          pos -= 1
        }
        return setValue(context, totalBig.longValue(), position, pos)
      }
      return setValue(context, total, position, pos)
    }

    def setValue(context: DateTimeParseContext,
                 value: Long,
                 errorPos: Int,
                 successPos: Int): Int =
      context.setParsedField(field, value, errorPos, successPos)

    override def toString(): String = {
      if (minWidth == 1 && maxWidth == 19 && signStyle == SignStyle.NORMAL) {
        return s"Value($field)"
      }
      if (minWidth == maxWidth && signStyle == SignStyle.NOT_NEGATIVE) {
        return s"Value($field,$minWidth)"
      }
      return s"Value($field,$minWidth,$maxWidth,$signStyle)"
    }
  }

  object NumberPrinterParser {
    val EXCEED_POINTS = Array[Int](
      0,
      10,
      100,
      1000,
      10000,
      100000,
      1000000,
      10000000,
      100000000,
      1000000000
    )
  }

}
