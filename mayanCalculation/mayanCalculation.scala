import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
    val Array(l, h) = for(i <- readLine split " ") yield i.toInt
    val mayaDigits=Array.ofDim[String](20)
    for (i <- 0 until 20) mayaDigits(i)=" " * (l*h)
    for(i <- 0 until h) {
        val numeral = readLine
        numeral.indices.foreach( idx=> 
            mayaDigits(idx / l) = mayaDigits(idx / l).updated((idx%l) + (i*l), numeral(idx))
        )
    }
    val s1 = readInt
    val n1:Long = readNumber(s1)
    val s2 = readInt
    val n2:Long = readNumber(s2)
    val operation = readLine

    val result = operation match {
        case "+" => n1 + n2
        case "-" => n1 - n2
        case "*" => n1 * n2
        case "/" => n1 / n2
    }
    println(intToMaya(result))
    
    def readNumber(numLines: Int)= {
        val base20number = for (nbDigits <- 0 until numLines / h) yield {
            val digit = (for(i <- 0 until h) yield readLine).mkString
            mayaToInt(digit)
        }
        var sum=0
        base20number.indices.foreach(
            idx => sum += pow(20, idx).toInt * base20number(base20number.size - idx - 1)
        )
        sum
    }

    def mayaToInt(data: String) =
        mayaDigits.indexOf(data)
        
    def intToMaya(number: Long) = {
        val mayaRepresentation = java.lang.Long.toString(number, 20)
        (for (c <- mayaRepresentation) yield
            mayaDigits(Integer.parseInt(""+c,20)).grouped(l).mkString("\n")
            ).mkString("\n")
    }
}