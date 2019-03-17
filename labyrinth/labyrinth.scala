import math._
import scala.util._
import scala.collection.mutable.ListBuffer

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
        def +(dx:Int, dy:Int)=
            Vec2(x+dx, y+dy)
        def +(v:Vec2)=
            Vec2(x+v.x, y+v.y)
        def -(v:Vec2)=
            Vec2(x-v.x, y-v.y)
    }

    // Phase 0 means "look for command center"
    // Phase 1 means "command center's location is known"
    // Phase 2 means "return to start"
    val PHASE_LOOK_FOR_CC = 0
    val PHASE_CC_SEEN = 1
    val PHASE_BACK_TO_START = 2

    val dUP = -c
    val dDOWN = c
    val dLEFT = -1
    val dRIGHT = 1
    
    val avDirs = Array(dUP, dDOWN, dLEFT, dRIGHT)
    val libDirs = Map(dUP->"UP", dDOWN->"DOWN", dLEFT->"LEFT", dRIGHT->"RIGHT")
    
    val deltaPos=Map("UP"->Vec2(0,-1), "DOWN"->Vec2(0,1),
                     "LEFT"->Vec2(-1,0), "RIGHT"->Vec2(1,0))
                     
    def nextPos(pos:Vec2, pas:Int)=
        Vec2(pos.x + pas%c, pos.y + pas / c)
        
    def insideLaby(pos:Vec2)=
        pos.x>=0 && pos.y>=0 && pos.x<c && pos.y<r
        
    def dirFromVec(v:Vec2)=
       ((deltaPos find (_._2==v)).get)._1
        
    // Return every possible move for a location on the form (dir, pos)
    def findAvailableDir(y: Int, x:Int)={
        for (d <- avDirs; pos=nextPos(Vec2(x, y), d)
            if insideLaby(pos)
            if labyrinth(pos.y)(pos.x) != '#')
        yield (libDirs(d), pos)
    }
    
    def distance(v1:Vec2, v2:Vec2)={
        val sum = math.pow(v2.x - v1.x, 2) +
                  math.pow(v2.y - v1.y, 2)
        math.sqrt(sum).intValue
    }
    
    // game loop
    var startPos:Vec2=Vec2(-1,-1)
    var ccPos:Vec2=Vec2(-1, -1)
    var lastMove:Option[String]=None
    var phase = PHASE_LOOK_FOR_CC
    var path=ListBuffer[Vec2]()
    val backPath=ListBuffer[String]()
    
    while(true) {
        // kr: row where Kirk is located.
        // kc: column where Kirk is located.
        val Array(kr, kc) = for(i <- readLine split " ") yield i.toInt
        val kirkPos = Vec2(kc, kr)
        if (path.isEmpty && phase == PHASE_LOOK_FOR_CC)
            path += Vec2(kc, kr)
            
        val isOnCommandCenter = labyrinth(kr)(kc) == 'C'
        
        Console.err.println("phase="+phase+" labycase="+labyrinth(kr)(kc)+" isCC="+isOnCommandCenter)
        if (isOnCommandCenter && phase < PHASE_BACK_TO_START) {
            phase = PHASE_BACK_TO_START
            lastMove = None
            // Clean path with only current location
            /*
            path.clear
            path += kirkPos
            */
            Console.err.println("Found command center !")
        }
        
        // Update labyrinth
        for(numRow <- 0 until r) {
            val row = readLine // C of the characters in '#.TC?' (i.e. one line of the ASCII maze).
            for (numCol <- 0 until c) {
                val c = row(numCol)
                labyrinth(numRow)(numCol) = c
                if (c == 'T')
                    startPos = Vec2(numRow, numCol)
                else if (c == 'C') {
                    ccPos = Vec2(numRow, numCol)
                    if (phase == PHASE_LOOK_FOR_CC) {
                        phase = PHASE_CC_SEEN
                    }
                }
            }
        }
        // Display labyrinth
        Console.err.println("Labyrinth:")
        for (row <- 0 until r)
        Console.err.println(labyrinth(row).mkString)
        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        
        // Find available direction
        val dirs=findAvailableDir(kr, kc)
        Console.err.println("last move="+lastMove)
        Console.err.println("path="+path)
        Console.err.println("dirs="+dirs.mkString)
        
        var chosen:(String,Vec2)=null
        if (phase == PHASE_CC_SEEN) {
            // Go to the command center, as we know where it is
            val initialDist = distance(kirkPos, ccPos)
            val dists = dirs map (d=>distance(d._2, ccPos))
            val minOne = dists.min
            val idxMin = dists indexOf minOne
            chosen = dirs(idxMin)
        }
        if (phase == PHASE_LOOK_FOR_CC && dirs.size == 3) {
            // Exploration
            Console.err.println("Explore !")
            val canKeepGoing = dirs filter (d=>d._1 == lastMove.get)
            if (canKeepGoing.size > 0) {
                chosen = (lastMove.get, kirkPos + (deltaPos(lastMove.get)) )
            }
        }
        if (phase == PHASE_BACK_TO_START) {
            // Follow the path reverse
            path = path dropRight 1
            val desiredLoc = path.last
            val delta = desiredLoc - kirkPos
            chosen = (dirFromVec(delta), desiredLoc)
            
        }
        if (chosen == null) {
            var firstOne = dirs find (d=>
                if (phase == PHASE_LOOK_FOR_CC)
                    !(path contains d._2) && d._2 != startPos
                else if (phase == PHASE_BACK_TO_START)
                    !(path contains d._2) && d._2 != ccPos
                else false
                )
            // Exception: (not sure if it's good to do that)
            // We allow Kirk to walk again on his track, but not on his very last location
            if (!(firstOne isDefined)) {
                val forbidden = path.dropRight(1).last
                firstOne = dirs find (d=>
                    if (phase == PHASE_LOOK_FOR_CC)
                        d._2 != forbidden && d._2 != startPos
                    else if (phase == PHASE_BACK_TO_START)
                        d._2 != forbidden && d._2 != ccPos
                    else false
                    )
            }
            chosen = firstOne.get
        }
        lastMove = Some(chosen._1)
        if (phase != PHASE_BACK_TO_START) {
            path += chosen._2
        }
        println(chosen._1) // Kirk's next move (UP DOWN LEFT or RIGHT).
    }
}