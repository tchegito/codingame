import math._
import scala.util._
import scala.collection.mutable.ListBuffer

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
    // n: the total number of nodes in the level, including the gateways
    // l: the number of links
    // e: the number of exit gateways
    val Array(n, l, e) = for(i <- readLine split " ") yield i.toInt
    
    case class Node(id: Int, var gateway: Boolean=false, var linked:ListBuffer[Node]=ListBuffer()) {
        override def toString():String=
            "Node[" + id + "] => [" + (linked map (n=>n.id)).mkString(",") + "]"
        def sever(linkedNode: Int) = {
            linked = linked filter ({
                case Node(id,_,_) if id == linkedNode =>false
                case _ => true
            })
        }
    }
    
    var nodes = new Array[Node](n)
    var nodesNbGateway = scala.collection.mutable.Map[Int, Int]()
    
    case class Link(a: Int, b: Int)
    
	val currentDistance = Array.fill(n)(10000)
	val currentScore = Array.fill(n)(1000)
	
	// Initialize all distance and score
	// Expect one element from currentDistance to be 0
    def fillDistances()={
		val unvisited:ListBuffer[Int] = ListBuffer(nodes map (i=>i.id): _ *)
		
		for (repeatNode <- unvisited) {
			var currentNode = -1
			var minDistance = 10000
			for (i <- currentDistance.indices) {
				if ((unvisited contains i) && minDistance > currentDistance(i)) {
					currentNode = i
					minDistance = currentDistance(i)
				}
			}
			// Neighbours
			for (node <- nodes(currentNode).linked if (unvisited contains node.id)) {
				val tempDistance = currentDistance(node.id)
				val curDistance = currentDistance(currentNode)
				if (tempDistance > curDistance+1) {
					currentDistance(node.id) = curDistance + 1
					currentScore(node.id) = currentScore(currentNode) + 1
					if (nodesNbGateway.getOrElse(node.id,-1) > 0) {
					    // Diminish score if there's a gateway close to the node
						currentScore(node.id) -= 1
					}
				}
			}
			unvisited -= currentNode
		}
	}

	var cacheDistance:Map[(Int,Int),Int]=Map()
	
	def distance(start:Int, end:Int, path:Seq[Int]=Seq()):Int={
		val alreadyComputed = cacheDistance.getOrElse((start,end), 0)
		if (alreadyComputed != 0)
			alreadyComputed
		else {
			val lookForEnd = nodes(start).linked find (n=>n.id == end)
			val result =
			if (lookForEnd isEmpty) {
				val paths = for (node <-nodes(start).linked if !(path contains node.id);
					dist = distance(node.id, end, path :+ node.id);
					if dist != -1)
				yield 1+dist
				//println("dist "+start+" to "+end+"="+paths)
				if (paths isEmpty) -1 else paths.min
			} else 1
			if (result != -1) {
			    cacheDistance += (start,end) -> result
			}
			result
		}
	}
	
    // Return (nodeId, pathLength)
    def findShortestPath(start:Int, end:Int, path:Seq[Int]=Seq()):(Int,Int)={
		val newPath = if (path isEmpty) Seq(start) else path
        // 1) find an end
        
        val curNode = nodes(start)
        val wayOut = for (node <- curNode.linked if node.id == end)
        yield node

    	if (wayOut isEmpty) {
    	    // 2) no end, so find a way to
			if (path.size > 1 && path.size > distance(path.head, end)) {
				return (-1,-1)
			}
			
            val allPaths = for (node <- curNode.linked if !path.contains(node.id);
				computed = findShortestPath(node.id, end, newPath :+ node.id)
				if computed._1 != -1)
			yield computed
            
            // Return shortest
            if ((allPaths size) > 0) {
                val lens =allPaths flatMap ({
                    case(nodeId,len) if len != -1 => List(len)
                    case _ => Nil })
                if (lens.size > 0) {
                    val shortest = lens.min
                    val idxShortest = lens indexOf shortest
                    return allPaths(idxShortest)
                }
            }
    		(-1,-1)
	    } else if (path isEmpty) {
			(wayOut.head.id, 1)
		} else {
			(path.drop(1).head, path.size)
		}
    }

    def selectMinPath(paths: Seq[(Int,Int, Int)]):(Int,Int)={
        if ((paths size) > 0) {
            val lens =paths flatMap ({
                case (nodeId,len, gateId) if len != -1 => List(len) 
                case _ => Nil})
            val shortest = lens.min
            val idxShortest = lens indexOf shortest
            var elemShortest = paths(idxShortest)
            return (elemShortest._1, elemShortest._3)
        }
        (-1,-1)
    }

    nodes = Array.tabulate(n)(n=>Node(n))
    
    for(i <- 0 until l) {
        // n1: N1 and N2 defines a link between these nodes
        val Array(n1, n2) = for(i <- readLine split " ") yield i.toInt
        
        val node = nodes(n1)
        node.linked += nodes(n2)
        nodes(n2).linked += node
    }
    Console.err.println("nodes="+nodes) 
    
    var gateways = scala.collection.mutable.ListBuffer[Int]()
    for(i <- 0 until e) {
        val ei = readInt // the index of a gateway node
        // TODO: maybe use 'yield'
        gateways += ei
        nodes(ei).gateway = true
        for (l <- nodes(ei).linked) {
            val cur = nodesNbGateway.getOrElse(l.id, -1)
            nodesNbGateway(l.id) = if (cur == -1) 1 else cur+1
        }
    }

    // game loop
    while(true) {
        val si = readInt // The index of the node on which the Skynet agent is positioned this turn
        Console.err.println("agent is at "+si)
        Console.err.println("gateways="+gateways)

        // Init distances and score        
        for (i<-0 until n) {
            currentDistance(i)=1000
            currentScore(i)=1000 
        }
        currentDistance(si) = 0
        currentScore(si) = 0
        
        fillDistances()
        
        // Find links containing agent position
        val toRemoveNodes = nodes(si).linked
        // 1) Choose one linked to a gateway
        var chosenNodes = toRemoveNodes find (n=>n.gateway)
        var chosens = chosenNodes map (n=>Link(si, n.id))
        Console.err.println("chosens="+chosens)
        // 2) Choose a node linked to at least 2 gateway
        val riskyNodes = nodes filter (n=>(n.linked filter (n=>n.gateway)).size > 1)
        Console.err.println("Found "+riskyNodes.size+" risky nodes")
        // 3) select node with lowest score
        if (chosens.isEmpty) {
            var minCurrentScore = 1000
            for ((nodeId,nbExits) <- nodesNbGateway) {
                val score = currentScore(nodeId) - nbExits
                if (minCurrentScore >= score) {
                    minCurrentScore = score
                    for (nod <- nodes(nodeId).linked if nod.gateway) {
                        chosens = Some(Link(nodeId, nod.id))
                    }
                }
            }
        }

        val chosenOne:Link =
            if (chosens.isEmpty) { // We have to find a way
                // shortest from all gateways
                Console.err.println("Try to find path")
                val allPaths = for (gate <- gateways;
                    paths =findShortestPath(gate, si, Seq())
                    if paths._1 != -1
                ) yield { 
                    (paths._1, paths._2, gate)
                }
                Console.err.println("Found "+allPaths.size+" ways => "+allPaths)
                val duet = selectMinPath(allPaths)
                Link(duet._2, duet._1)
            } else
                chosens.head
        Console.err.println("on coupe "+chosenOne)
        // Remove link
        nodes(chosenOne.a).sever(chosenOne.b)
        nodes(chosenOne.b).sever(chosenOne.a)
        // (we don't know which a or b is the gateway
        if (gateways contains chosenOne.a) {
            nodesNbGateway(chosenOne.b) -= 1
        } else {
            nodesNbGateway(chosenOne.a) -= 1
        }            

        // Clear cache, because distance already computed becomes
        // obsolete after link being severed
        cacheDistance = Map()
        
        // Update gateway (not necessary)
        for (a <- gateways)
            if (nodes(a).linked isEmpty)
                gateways remove (gateways indexOf a)
        println(chosenOne.a+" "+chosenOne.b)
    }
}