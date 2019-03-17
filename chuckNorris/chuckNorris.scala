import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
    val message = readLine
    
    def encodeChar(message: String) = {
        val binary = (message map (
            c => String.format("%7s", c.toBinaryString).replace(" ", "0")
            )).mkString
        val sequences = binary.replaceAll("10", "1,0")
            .replaceAll("01", "0,1")
            .split(",")
        (for (str <- sequences) yield {
            (if (str(0) == '0') "00" else "0") + " " + "0"*str.length
        }).mkString(" ")
    }
        
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    
   print(encodeChar(message))
}