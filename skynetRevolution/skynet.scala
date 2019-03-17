import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
    // n: the total number of nodes in the level, including the gateways
    // l: the number of links
    // e: the number of exit gateways
    val Array(n, l, e) = for(i <- readLine split " ") yield i.toInt
    
    case class Link(a: Int, b: Int)
    
    var links = List[Link]()
        
    def findLinkWith(a: Int) =
        links filter ( {
            case Link(n1, n2) => n1 == a || n2 == a
        } )

    def findPath(start:Int, end:Int, path:Seq[Int]):Int={
        Console.err.println("find path from "+start+" to "+end)
        // 1) find an end
    	val wayOut= for (lnk <- links 
    	    if lnk.a == start && lnk.b == end
    	    || lnk.a == end && lnk.b == start)
    		yield lnk
    	if (wayOut isEmpty) {
    	    // 2) no end, so find a way to
    	    Console.err.println("lookin into links "+links.size+" with current path "+path)
            val linked = findLinkWith(start)
            val attempted =(for (lnk <- linked 
                if (lnk.a == start && !(path contains lnk.b)))
            yield lnk.b) ++
            (for (lnk <- linked
                if (lnk.b == start && !(path contains lnk.a)))
            yield lnk.a)
            attempted.foreach{
                i=> val v = findPath(i, end, path :+ i) 
                    if (v != -1) return v
            }
    		-1
	    } else path.head
    }
            
    for(i <- 0 until l) {
        // n1: N1 and N2 defines a link between these nodes
        val Array(n1, n2) = for(i <- readLine split " ") yield i.toInt
        links = Link(n1, n2) :: links
    }
    Console.err.println("links="+links)
    
    var gateways = scala.collection.mutable.ListBuffer[Int]()
    for(i <- 0 until e) {
        val ei = readInt // the index of a gateway node
        // TODO: maybe use 'yield'
        gateways += ei
    }

    // game loop
    while(true) {
        val si = readInt // The index of the node on which the Skynet agent is positioned this turn
        Console.err.println("agent is at "+si)
        Console.err.println("gateaways="+gateways)
        // Find links containing agent position
        val toRemoveLinks = findLinkWith(si)
        // Choose one linked to a gateway
        val chosens = (toRemoveLinks find( {
            case Link(n1, n2) => (gateways contains n1) ||
                                 (gateways contains n2)
        }))
        val chosenOne:Link =
            if (chosens.isEmpty) // We have to find a way
                Link(si, findPath(si, gateways.head, Seq()))
            else
                chosens.head
        Console.err.println("on coupe "+chosenOne)
        // Remove link (not necessary)
        links = links filter ( _ != chosenOne)
        // Update gateway (not necessary)
        for (a <- gateways)
            if (findLinkWith(a) isEmpty)
                gateways remove (gateways indexOf a)
        println(chosenOne.a+" "+chosenOne.b)
    }
}