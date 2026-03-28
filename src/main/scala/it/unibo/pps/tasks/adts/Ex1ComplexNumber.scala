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
      
      def re(): Double = complex match
        case ComplexNumber(real, _) => real
        
      def im(): Double = complex match
        case ComplexNumber(_, imaginary) => imaginary
        
      def sum(other: Complex): Complex = (complex, other) match
        case (ComplexNumber(re1, im1), ComplexNumber(re2, im2)) => ComplexNumber(re1 + re2, im1 + im2)
          
      def subtract(other: Complex): Complex = (complex, other) match
        case (ComplexNumber(re1, im1), ComplexNumber(re2, im2)) => ComplexNumber(re1 - re2, im1 - im2)
          
      def asString(): String = complex match
        case ComplexNumber(re, im) if re != 0.0 || im != 0.0 => 
          s"${_realAsString()}${_signAsString()}${_imaginaryAsString()}"
        case ComplexNumber(re, _) => s"$re"
        
      private def _realAsString(): String = complex match
        case ComplexNumber(re, _) if re != 0.0 => s"$re"
        case _ => ""
        
      private def _signAsString(): String = complex match
        case ComplexNumber(re, im) if re != 0.0 && im > 0.0 => " + "
        case ComplexNumber(re, im) if re != 0.0 && im < 0.0 => " - "
        case ComplexNumber(re, im) if re == 0.0 && im < 0.0 => "-"
        case _ => ""
        
      private def _imaginaryAsString(): String = complex match
        case ComplexNumber(_, im) if im > 0.0 => s"${im}i"
        case ComplexNumber(_, im) if im < 0.0 => s"${-im}i"
        case _ => ""
