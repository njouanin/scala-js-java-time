package java.time.temporal

import java.time.{LocalDate, LocalTime, ZoneId, ZoneOffset}
import java.time.chrono.Chronology
import java.time.temporal.ChronoField._

object TemporalQueries {
  final def zoneId(): TemporalQuery[ZoneId] = ZONE_ID
  final val ZONE_ID = new TemporalQuery[ZoneId]() {
  import java.time.ZoneId
    override def queryFrom(temporal: TemporalAccessor):ZoneId = temporal.query(this)
  }

  final def chronology():TemporalQuery[Chronology] = CHRONO
  final val CHRONO = new TemporalQuery[Chronology]() {
    override def queryFrom(temporal: TemporalAccessor):Chronology = temporal.query(this)
  }

  final def precision():TemporalQuery[TemporalUnit] = PRECISION
  final val PRECISION = new TemporalQuery[TemporalUnit]() {
    override def queryFrom(temporal: TemporalAccessor):TemporalUnit  = temporal.query(this)
  }

  final def zone():TemporalQuery[ZoneId] = ZONE
  final val ZONE = new TemporalQuery[ZoneId]() {
    override def queryFrom(temporal: TemporalAccessor):ZoneId = {
      val zone = temporal.query(ZONE_ID)
      if(zone != null) zone else temporal.query(OFFSET)
    }
  }

  def offset():TemporalQuery[ZoneOffset] = OFFSET
  final val OFFSET = new TemporalQuery[ZoneOffset]() {
    override def queryFrom(temporal: TemporalAccessor):ZoneOffset = {
      if (temporal.isSupported(OFFSET_SECONDS)) {
        ZoneOffset.ofTotalSeconds(temporal.get(OFFSET_SECONDS))
      }
      else
        null
    }
  }

  final def localDate():TemporalQuery[LocalDate] = LOCAL_DATE
  final val LOCAL_DATE = new TemporalQuery[LocalDate]() {
    override def queryFrom(temporal: TemporalAccessor):LocalDate = {
      if (temporal.isSupported(EPOCH_DAY)) {
        LocalDate.ofEpochDay(temporal.getLong(EPOCH_DAY))
      }
      else
        null
    }
  }

  final def localTime():TemporalQuery[LocalTime] = LOCAL_TIME
  final val LOCAL_TIME = new TemporalQuery[LocalTime]() {
    override def queryFrom(temporal: TemporalAccessor):LocalTime = {
      if (temporal.isSupported(NANO_OF_DAY)) {
        LocalTime.ofNanoOfDay(temporal.getLong(NANO_OF_DAY))
      }
      else
        null
    }
  }
}