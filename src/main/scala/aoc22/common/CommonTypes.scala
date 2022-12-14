package aoc22.common

import scala.annotation.targetName

// Extension pair -> Point2D
extension (p: (Int, Int))
  def pos: Point2D = Point2D(p(0), p(1))

// 2D point, vector, or complex number type
case class Point2D(x: Int, y: Int):
  // Construct from pair
  def this(p: (Int, Int)) = this(p(0), p(1))

  // Convert to pair
  def pair: (Int, Int) = (x, y)

  // Vector / complex addition and subtraction
  @targetName("add")
  def +(other: Point2D): Point2D = Point2D(x + other.x, y + other.y)

  @targetName("subtract")
  def -(other: Point2D): Point2D = Point2D(x - other.x, y - other.y)

  // Scalar multiplication
  @targetName("mult")
  def *(scalar: Int): Point2D = Point2D(x * scalar, y * scalar)

  // Distance calculations
  def hdist(otherX: Int): Int = Math.abs(otherX - x)
  def hdist(other: Point2D): Int = hdist(other.x)

  def vdist(otherY: Int): Int = Math.abs(otherY - y)
  def vdist(other: Point2D): Int = vdist(other.y)

  def manhattan(other: Point2D): Int = hdist(other) + vdist(other)

  def magnitude: Double = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2))

  // Check if in bounds
  def inBounds(minBound: Point2D, maxBound: Point2D): Boolean =
    val Point2D(minX, minY) = minBound
    val Point2D(maxX, maxY) = maxBound
    x >= minX && x <= maxX && y >= minY && y <= maxY

  def constrainToBounds(minBound: Point2D, maxBound: Point2D): Point2D =
    Point2D(Math.max(minBound.x, Math.min(maxBound.x, x)), Math.max(minBound.y, Math.min(maxBound.y, y)))

  // Modulo the components independently, useful for wrapping
  // named floorMod rather than % because it does sane (Math.floorMod) handling of negatives
  def floorMod(modulus: Point2D): Point2D =
    Point2D(Math.floorMod(x, modulus.x), Math.floorMod(y, modulus.y))