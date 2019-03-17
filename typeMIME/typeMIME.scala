import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
    val n = readInt // Number of elements which make up the association table.
    val q = readInt // Number Q of file names to be analyzed.
    val associations = (
    for(i <- 0 until n) yield {
        // ext: file extension
        // mt: MIME type.
        val Array(ext, mt) = readLine split " "
        (ext, mt)
    }
    )
    for(i <- 0 until q) {
        val fname = readLine // One file name per line.
        val posPoint = fname.lastIndexOf(".")
        val extension = if (posPoint == -1) "" else fname.substring(posPoint+1)
        val matching = associations.find( {
            case (ext, mt) => ext equalsIgnoreCase extension
        } )
        println( if (matching.isEmpty) "UNKNOWN" else matching.get._2 )
    }
    
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    

    // For each of the Q filenames, display on a line the corresponding MIME type. If there is no corresponding type, then display UNKNOWN.
    //println("UNKNOWN")
}