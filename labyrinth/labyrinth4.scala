import math._
import scala.util._
import scala.collection.mutable.{ListBuffer, Stack, LinkedHashSet}

// Strongly based on:
// https://github.com/texus/codingame/blob/master/SingePlayer/Hard/The%20Labyrinth.cpp

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
    // r: number of rows.
    // c: number of columns.
    // a: number of rounds between the time the alarm countdown is activated and the time the alarm goes off.
    val Array(r, c, a) = for(i <- readLine split " ") yield i.toInt

    val labyrinth = Array.ofDim[Char](r, c)

    case class Vec2(x:Int, y:Int) {
        def +(v:Vec2) = Vec2(x+v.x,y+v.y)
        def unary_- = Vec2(-this.x,-this.y)
        def -(v:Vec2) = this + -v
    }

    val deltaPos=Map("UP"->Vec2(0,-1), "DOWN"->Vec2(0,1),
                 "LEFT"->Vec2(-1,0), "RIGHT"->Vec2(1,0))
        
    def insideLaby(pos:Vec2)=
        pos.x>=0 && pos.y>=0 && pos.x<c && pos.y<r
    
    // Really close to findAvailableDir, but without "libDir", and return
    // empty list when location is on a unidentified place
    def getNeighbors(pos:Vec2)={
        if (labyrinth(pos.y)(pos.x) == '?')
            List()
        else
            for (d <- deltaPos.values; loc=pos + d
                if insideLaby(loc)
                if labyrinth(loc.y)(loc.x) != '#')
            yield loc
    }
    
    def dijkstra(source:Vec2, goal:Char):List[Vec2]={
    	val dist = Array.fill[Int](r, c)(Int.MaxValue)
    	val prev = Array.fill[Vec2](r, c)(Vec2(Int.MaxValue, Int.MaxValue))
    	val unvisited=ListBuffer[Vec2]()
    	
    	// Init
    	dist(source.y)(source.x) = 0
    	for (y<-0 until r; x<-0 until c) {
    		unvisited += Vec2(x, y)
    	}
    	
    	while (!unvisited.isEmpty) {
    		// Find node with smallest distance
    		var minDist = Int.MaxValue
    		var minNode = Vec2(-1, -1)
    		for (v<-unvisited) {
    			if (dist(v.y)(v.x) < minDist) {
    				minNode = v
    				minDist = dist(v.y)(v.x)
    			}
    		}
    		// Return an empty list when there is no way to get to the goal
    		if (minDist == Int.MaxValue) {
    			return List()
    		}
    		// Remove and return best vertex
    		unvisited -= minNode
    		
    		// Check if goal node
    		if (labyrinth(minNode.y)(minNode.x) == goal) {
    			val path=ListBuffer[Vec2]()
    			var node = minNode
    			while (prev(node.y)(node.x).x != Int.MaxValue &&
    			       prev(node.y)(node.x).y != Int.MaxValue) {
    			       path prepend node
    			       node = prev(node.y)(node.x)
    			}
    			
    			return path.toList
    		}
    		
    		val neighbors = getNeighbors(minNode)
    		//Console.err.println("neighbors="+neighbors)
    		val alt = dist(minNode.y)(minNode.x) +1
    		for (n <- neighbors) {
    			if (alt < dist(n.y)(n.x)) {
    				dist(n.y)(n.x) = alt
    				prev(n.y)(n.x) = minNode
    			}
    		}
    	}
    	List()
    }
    
    // game loop
    var ccVisited = false
    
    while(true) {
        // kr: row where Kirk is located.
        // kc: column where Kirk is located.
        val Array(kr, kc) = for(i <- readLine split " ") yield i.toInt
        val kirkPos = Vec2(kc, kr)

        val isOnCommandCenter = labyrinth(kr)(kc) == 'C'
        
        Console.err.println("labycase="+labyrinth(kr)(kc)+" isCC="+isOnCommandCenter)
        if (isOnCommandCenter) {
            Console.err.println("Found command center !")
            ccVisited = true
        }
        
        // Update labyrinth
        for(numRow <- 0 until r) {
            val row = readLine // C of the characters in '#.TC?' (i.e. one line of the ASCII maze).
            for (numCol <- 0 until c) {
                val c = row(numCol)
                labyrinth(numRow)(numCol) = c
            }
        }
        // Display labyrinth
        Console.err.println("Labyrinth:")
        for (row <- 0 until r)
        Console.err.println(labyrinth(row).mkString)
        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        
        val path =
            if (ccVisited)
                dijkstra(kirkPos, 'T')
            else {
                val tryPath = dijkstra(kirkPos, 'C')
                if (tryPath isEmpty)
                    dijkstra(kirkPos, '?')
                else
                    tryPath
            }
        Console.err.println("Found path="+path)
        
        val node = path.head
        val dir =
            if (node.x > kirkPos.x) "RIGHT"
            else if (node.x < kirkPos.x) "LEFT"
            else if (node.y < kirkPos.y) "UP"
            else "DOWN"
        
        println(dir) // Kirk's next move (UP DOWN LEFT or RIGHT).
    }
}