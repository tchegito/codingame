import math._
import scala.util._

/**
 * Don't let the machines win. You are humanity's last hope...
 **/
object Player extends App {
    val width = readInt // the number of cells on the X axis
    val height = readInt // the number of cells on the Y axis
    val grille = Array.ofDim[Char](height, width)
    Console.err.println("taille="+width+"/"+height)
    for(i <- 0 until height) {
        val line = readLine // width characters, each either 0 or .
        Console.err.println("oneline="+line)
        line.indices.foreach (
            idx => grille(i)(idx) = line(idx)
        )
    }
    Console.err.println("grille = "+grille)
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    
    def find(x:Int, y:Int) ={
        //Console.err.println("grille "+x+","+y+"="+grille(y)(x))
        if (y >= height ||x >= width)
            Seq(-1, -1)
        else if (grille(y)(x) == '.')
            Seq(-1, -1)
        else
            Seq(x, y)
    }

    def findVertical(x:Int, y:Int):Seq[Int] ={
        if (y >= height || x >= width)
            Seq(-1, -1)
        else if (grille(y)(x) == '.')
            findVertical(x, y+1)
        else
            Seq(x, y)
    }
    def findHorizontal(x:Int, y:Int):Seq[Int] ={
        if (y >= height || x >= width)
            Seq(-1, -1)
        else if (grille(y)(x) == '.')
            findHorizontal(x+1, y)
        else
            Seq(x, y)
    }
    // Three coordinates: a node, its right neighbor, its bottom neighbor
    for (y <- 0 until height) {
        for (x <- 0 until width) {
            val current = find(x, y)
            if (current != Seq(-1,-1)) {
                var answer=scala.collection.mutable.ListBuffer[Int]()
                answer ++= find(x, y)
                answer ++= findHorizontal(x+1, y)
                answer ++= findVertical(x, y+1)
                Console.err.println("On renvoie "+answer)
                println(answer.mkString(" "))
            }
        }
    }
    
}