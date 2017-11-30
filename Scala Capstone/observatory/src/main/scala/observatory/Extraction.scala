package observatory

import java.time.LocalDate

import org.apache.spark.sql._
import utils.SparkJob
import utils.Resources._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.{DoubleType, IntegerType}

/**
  * 1st milestone: data extraction
  */
object Extraction extends SparkJob{

  import spark.implicits._

  def stations(stationsFile: String): Dataset[Station] = {
    spark
      .read
      .csv(resourcePath(stationsFile))
      .select(
        concat_ws("~", coalesce('_c0, lit("")), '_c1).alias("id"),
        '_c2.alias("latitude").cast(DoubleType),
        '_c3.alias("longitude").cast(DoubleType)
      )
      .where('_c2.isNotNull && '_c3.isNotNull && '_c2 =!= 0.0 && '_c3 =!= 0.0)
      .as[Station]
  }

  def temperatures(year: Year, temperaturesFile: String): Dataset[TemperatureRecord] = {
    spark
      .read
      .csv(resourcePath(temperaturesFile))
      .select(
        concat_ws("~", coalesce('_c0, lit("")), '_c1).alias("id"),
        '_c3.alias("day").cast(IntegerType),
        '_c2.alias("month").cast(IntegerType),
        lit(year).as("year"),
        (('_c4 - 32)/9 * 5).alias("temperature").cast(DoubleType)
      )
      .where('_c4.between(-500, 500))
      .as[TemperatureRecord]
  }



  def combineStationTemperature(stations: Dataset[Station], temperatures: Dataset[TemperatureRecord]): Dataset[StationTemperatureCombined] = {
    stations
      .join(temperatures, usingColumn = "id")
      .as[Combined]
      .map(x => (StationDate(x.day, x.month, x.year), Location(x.latitude, x.longitude), x.temperature))
      .toDF("date", "location", "temperature")
      .as[StationTemperatureCombined]
  }
    /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val combined = combineStationTemperature(stations(stationsFile), temperatures(year, temperaturesFile))
    combined.collect().par
      .map(c => (c.date.toLocalDate, c.location, c.temperature))
        .seq
  }


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.par
      .groupBy(_._2)
      .mapValues(temperature =>
        temperature.foldLeft(0.0)((sum, t) => sum + t._3)/temperature.size)
      .seq

  }
}
