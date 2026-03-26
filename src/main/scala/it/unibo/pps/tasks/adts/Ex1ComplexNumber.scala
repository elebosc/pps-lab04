package it.unibo.pps.tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumber:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    case class ComplexNumber(real: Double, imaginary: Double)

    type Complex = ComplexNumber
    def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)
    extension (complex: Complex)
      def re(): Double = complex.real
      def im(): Double = complex.imaginary
      def sum(other: Complex): Complex = ComplexNumber(complex.real + other.real, complex.imaginary + other.imaginary)
      def subtract(other: Complex): Complex = ComplexNumber(complex.real - other.real, complex.imaginary - other.imaginary)
      def asString(): String = complex match
        case c if c.real != 0.0 || c.imaginary != 0.0 => s"${_realAsString()}${_signAsString()}${_imaginaryAsString()}"
        case _ => s"${complex.real}"
      private def _realAsString(): String = complex.real match
        case re if re != 0.0 => s"$re"
        case _ => ""
      private def _signAsString(): String = complex match
        case c if c.real != 0.0 && c.imaginary > 0.0 => " + "
        case c if c.real != 0.0 && c.imaginary < 0.0 => " - "
        case c if c.real == 0.0 && c.imaginary < 0.0 => "-"
        case _ => ""
      private def _imaginaryAsString(): String = complex.imaginary match
        case im if im > 0.0 => s"${im}i"
        case im if im < 0.0 => s"${-im}i"
        case _ => ""
