package observatory

import java.time.LocalDate

import com.sksamuel.scrimage.RGBColor

import scala.math._

case class Station(id: String, latitude: Double, longitude: Double)
case class TemperatureRecord(id: String, day: Int, month: Int, year: Int, temperature: Double)
case class StationDate(day: Int, month: Int, year: Int){
  def toLocalDate = LocalDate.of(year, month, day)
}
case class Combined(id: String, latitude: Double, longitude: Double, day: Int, month: Int, year: Int, temperature: Double)
case class StationTemperatureCombined(date: StationDate, location: Location, temperature: Double)
/**
  * Introduced in Week 1. Represents a location on the globe.
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  lazy val point: Point  = Point(toRadians(lat), toRadians(lon))
}
case class Point(lat: Double, lon: Double) {
  lazy val location: Location = Location(toDegrees(lat), toDegrees(lon))

  def haversineEarthDistance(otherPoint: Point): Double = {
    6372.8 * greatCircleDistance(otherPoint) * 1000
  }

  def greatCircleDistance(otherPoint: Point): Double = {
    val deltaLat = abs(otherPoint.lat - lat)
    val deltaLon = abs(otherPoint.lon - lon)

    val a = pow(sin(deltaLat/2), 2) + cos(lat) * cos(otherPoint.lat) * pow(sin(deltaLon/2), 2)
    2 * atan2(sqrt(a), sqrt(1-a))
  }
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Double, y: Double, zoom: Int) {
  lazy val location: Location = Location(
    lat = atan(sinh(Pi * (1 - 2 * y/pow(2.0, zoom)))) * 180.0 / Pi,
    lon = 360.0 * x/pow(2.0, zoom) - 180.0
  )
  def toURI = new java.net.URI("http://tile.openstreetmap.org/" + zoom + "/" + x + "/" + y + ".png")
}
/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)



/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int) {
  def pixel(alpha: Int) = RGBColor(red, green, blue, alpha).toPixel
}

