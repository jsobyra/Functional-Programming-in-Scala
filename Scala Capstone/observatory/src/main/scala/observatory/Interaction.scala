package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization._
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = tile.location


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val width = 256
    val height = 256

    val pixels = (0 until width * height).par
      .map(
        position => {
          val xPosition = (position % width).toDouble / width + tile.x
          val yPosition = (position / height).toDouble / height + tile.y

          position -> interpolateColor(
            colors,
            predictTemperature(temperatures, Tile(xPosition, yPosition, tile.zoom).location)
          ).pixel(127)
        }
      )
      .seq
      .sortBy(_._1)
      .map(_._2)

    Image(width, height, pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    val x = for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } {
      generateImage(year, Tile(x, y, zoom), data)
    }
  }

}
