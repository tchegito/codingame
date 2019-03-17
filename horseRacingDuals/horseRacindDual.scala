import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
    val n = readInt
    val data = for (i <- 1 to n) yield readInt
    
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    
    Console.err.println("On a "+n+" puissances")
    var closest=9999
    var sortedData = data.sorted
    for (i<-0 to n-2) {
        closest = Math.min(closest, sortedData(i+1) - sortedData(i))
    }
    
    println(closest)
}