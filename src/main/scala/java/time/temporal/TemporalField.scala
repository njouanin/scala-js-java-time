package java.time.temporal

import java.time.format.ResolverStyle
import java.util.Locale

trait TemporalField {
  // Not implemented
  // def getDisplayName(locale: java.util.Locale): String

  def getBaseUnit(): TemporalUnit

  def getRangeUnit(): TemporalUnit

  def range(): ValueRange

  def isDateBased(): Boolean

  def isTimeBased(): Boolean

  def isSupportedBy(temporal: TemporalAccessor): Boolean

  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange

  def getFrom(temporal: TemporalAccessor): Long

  def getDisplayName(locale: Locale): String

  def adjustInto[R <: Temporal](temporal: R, value: Long): R

  def resolve(fieldValues: java.util.Map[TemporalField, Long],
              partialTemporal: TemporalAccessor,
              resolverStyle: ResolverStyle): TemporalAccessor
}
