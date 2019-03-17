import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
    val l = readInt
    val h = readInt
    val t = readLine
    val fontData = new Array[String](h)
    for(i <- 0 until h) {
        fontData(i) = readLine
    }

    def charPos(lettre: Char) = {
        val fonts = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"
        var c = fonts.indexOf(lettre.toUpper)
        if (c == -1)
            fonts.indexOf("?")
        else
            c
    }
    
    val seqCharText  = for (lettre <- t) yield charPos(lettre)

    val textRow = Array.fill[String](h)("")
    for (row <- 0 until h) {
        for (charPos <- seqCharText) {    
            textRow(row) += fontData(row).substring(charPos * l, charPos * l + l)
        }
        println(textRow(row))
    }
}