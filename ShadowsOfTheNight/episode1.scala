import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
    // w: width of the building.
    // h: height of the building.
    val Array(w, h) = for(i <- readLine split " ") yield i.toInt
    val n = readInt // maximum number of turns before game over.
    val Array(x0, y0) = for(i <- readLine split " ") yield i.toInt

    case class Vec2(x: Int, y:Int)
    
    object Vec2 {
        def fromStr(dir: String)=
            dir match {
                case "U" => Vec2(0,-1)
                case "UR" => Vec2(1,-1)
                case "UL" => Vec2(-1,-1)
                case "L" => Vec2(-1,0)
                case "R" => Vec2(1,0)
                case "DR" => Vec2(1,1)
                case "DL" => Vec2(-1,1)
                case  "D" => Vec2(0,1)
            }
    }
    
    // game loop
    var batPos = Vec2(x0, y0)
    var (minH, maxH) = (0, h)
    var (minW, maxW) = (0, w)
    while(true) {
        val bombdir = readLine // the direction of the bombs from batman's current location (U, UR, R, DR, D, DL, L or UL)
        
        val dir = Vec2.fromStr(bombdir)
        // Adjust minima
        if (dir.y < 0) 
            maxH = batPos.y 
        else if (dir.y > 0)
            minH = batPos.y
        if (dir.x < 0)
            maxW = batPos.x
        else if (dir.x > 0)
            minW = batPos.x
        Console.err.println("revised building=(%d,%d,%d,%d)".format(minW,minH,maxW,maxH))
        Console.err.println("Direction="+dir)
        
        val nx = if (dir.x < 0 ) (minW+batPos.x) / 2 
            else if (dir.x > 0 ) (maxW+batPos.x) / 2 
            else batPos.x 
        val ny = if (dir.y < 0) (minH+batPos.y) / 2
            else if (dir.y > 0) (maxH+batPos.y) / 2
            else batPos.y
        val nextPos = Vec2(nx, ny)
        Console.err.println("nextpos = "+nextPos)
        
        batPos = nextPos

        // the location of the next window Batman should jump to.
        println("%d %d".format(nextPos.x, nextPos.y))
    }
}