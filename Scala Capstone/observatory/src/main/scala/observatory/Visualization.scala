package observatory

import com.sksamuel.scrimage.Image

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  def distanceTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Iterable[(Double, Double)] = {
    temperatures
      .map {
        case (other, temperature) => (location.point.haversineEarthDistance(other.point), temperature)
      }
  }

  def inverseDistanceWeighted(distanceTemperature: Iterable[(Double, Double)], power: Int): Double = {
    val (weightedSum, inverseWeightedSum) = distanceTemperature
      .aggregate((0.0, 0.0))(
        {
          case ((ws, iws), (distance, temperature)) => {
            val w = 1/pow(distance, power)
            (w*temperature + ws, w + iws)
          }
        },
        {
          case ((ws1, iws1), (ws2, iws2)) => (ws1 + ws2, iws1 + iws2)
        }
      )

    weightedSum/inverseWeightedSum
  }

  def linearInterpolationValue(min: Double, max: Double, value: Double)(colorMin: Int, colorMax: Int): Int = {
    val factor = (value - min) / (max - min)

    round(colorMin + (colorMax - colorMin) * factor).toInt
  }

  def linearInterpolation(point1: Option[(Temperature, Color)], point2: Option[(Temperature, Color)], temperature: Temperature): Color =
    (point1, point2) match {
      case (Some((temperature1, color1)), Some((temperature2, color2))) => {
        val interpolation = linearInterpolationValue(temperature1, temperature2, temperature) _
        Color(
          interpolation(color1.red, color2.red),
          interpolation(color1.green, color2.green),
          interpolation(color1.blue, color2.blue)
        )
      }
      case (Some(point1), None) => point1._2
      case (None, Some(point2)) => point2._2
      case _ => Color(0, 0, 0)
    }

  def positionToLocation(imageWidth: Int, imageHeight: Int)(position: Int): Location = {
    val widthFactor = 180 * 2/imageWidth.toDouble
    val heightFactor = 90 * 2/imageHeight.toDouble

    val x = position%imageWidth
    val y = position/imageWidth

    Location(90 - (y * heightFactor), (x * widthFactor) - 180)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val predictions: Iterable[(Double, Double)] = distanceTemperature(temperatures, location)

    predictions.find(_._1 == 0.0) match {
      case Some((_, temperature)) => temperature
      case _ => inverseDistanceWeighted(predictions, 3)
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    points.find(_._1 == value) match {
      case Some((_, color)) => color
      case None => {
        val (smaller, greater) = points.toList.sortBy(_._1).partition(_._1 < value)
        linearInterpolation(smaller.reverse.headOption, greater.headOption, value)
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180

    val locationMapper = positionToLocation(width, height) _

    val pixels = (0 until height * width).par
      .map {
        pos => {
          pos -> interpolateColor(
            colors,
            predictTemperature(
              temperatures,
              locationMapper(pos)
            )
          ).pixel(255)
        }
      }
      .seq
      .sortBy(_._1)
      .map(_._2)
    Image(width, height, pixels.toArray)
  }

}

