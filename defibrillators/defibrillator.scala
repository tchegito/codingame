import math._
import scala.util._

object Solution extends App {
        
    def toCoord(s:String)=s.replaceAll(",", ".").toFloat
    
    val lon = toCoord(readLine)
    val lat = toCoord(readLine)
    val n = readInt
    
    // Map with values indexed by distance
    var data = Map[Double, String]()
    
    def distance(lo1:Double, lo2:Double, la1:Double, la2:Double) = {
        val deltaLo = lo1 - lo2
        val deltaLa = la1 - la2
        val x = deltaLo * Math.cos((la1+la2) / 2)
        val y = deltaLa
        Math.sqrt( deltaLo * deltaLo + deltaLa * deltaLa) * 6371
    }

    var miniDist = 90000.0
    for(i <- 0 until n) {
        val defib = readLine
        val str = defib.split(";")
        val dataLon = toCoord(str(4))
        val dataLat= toCoord(str(5))
        val dist = distance(dataLon, lon, dataLat, lat)
        data += (dist -> str(1))
        miniDist = Math.min(miniDist, dist)
    }
    
    val nearest = data(miniDist)
    Console.err.println("mini="+miniDist + "name="+nearest)
    
    println(nearest)
}