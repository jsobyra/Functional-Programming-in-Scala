package observatory

import observatory.Visualization.interpolateColor
import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._
/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {

    d00 * (1-point.x) * (1-point.y) +
    d10 * point.x * (1- point.y) +
    d01 * (1 - point.x) * point.y +
    d11 * point.x * point.y
  }


  def tileToPixels(tile: Tile, imageWidth: Int, imageHeight: Int): IndexedSeq[(Int, Location)] = {
    for {
      x <- 0 until imageWidth
      y <- 0 until imageHeight
    } yield x + y*imageWidth -> Tile(x.toDouble / imageWidth + tile.x, y.toDouble / imageHeight + tile.y, tile.zoom).location
  }


  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val width = 256
    val height = 256

    val pixels = tileToPixels(tile, width, height).par
      .map{
        case (position, location) => {
          val lat = List(floor(location.lat).toInt, ceil(location.lat).toInt)
          val lon = List(floor(location.lon).toInt, ceil(location.lon).toInt)

          val corners = {
            for {
              x <- 0 to 1
              y <- 0 to 1
            } yield (x, y) -> grid(GridLocation(lat(1-y), lon(x)))
          }.toMap

          val x = location.lon - lon(0)
          val y = lat(1) - location.lat

          position -> interpolateColor(
            colors,
            bilinearInterpolation(CellPoint(x, y), corners((0, 0)), corners((0, 1)), corners((1, 0)), corners((1, 1)))
          ).pixel(127)
        }
      }
      .seq
      .sortBy(_._1)
      .map(_._2)

    Image(width, height, pixels.toArray)
  }
}
