import math._
import scala.util._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashSet

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
    
    case class Path(from:Option[Vec2], 
        nodes:LinkedHashSet[Vec2], 
        to:ListBuffer[Path],
        var open:Boolean=true)
    
    // Phase 0 means "look for command center"
    // Phase 1 means "command center's location is known"
    // Phase 2 means "return to start"
    val PHASE_LOOK_FOR_CC = 0
    val PHASE_CC_SEEN = 1
    val PHASE_BACK_TO_START = 2
    val PHASE_REWIND_PATH = -1

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
    
    def isPathKnown(from:Vec2, first:Vec2)=
        (allPaths find (p=>p.from == Some(from) && p.nodes.head == first)).isDefined
        
    // game loop
    var startPos:Vec2=Vec2(-1,-1)
    var ccPos:Vec2=Vec2(-1, -1)
    var lastMove:Option[String]=None
    var phase = PHASE_LOOK_FOR_CC
    var path=ListBuffer[Vec2]()
    var backPath=List[Vec2]()
    var lastPos:Option[Vec2]=None

    var currentPath=Path(None, LinkedHashSet(), ListBuffer())
    var previousPath:Option[Path]=Some(currentPath)
    
    val allPaths=ListBuffer[Path]()
    
    while(true) {
        // kr: row where Kirk is located.
        // kc: column where Kirk is located.
        val Array(kr, kc) = for(i <- readLine split " ") yield i.toInt
        val kirkPos = Vec2(kc, kr)
        if (path.isEmpty && phase == PHASE_LOOK_FOR_CC)
            path += Vec2(kc, kr)
            
        // Update current path (if it's not closed)
        if (currentPath.open) {
            currentPath.nodes += kirkPos
        }
        
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
                    startPos = Vec2(numCol, numRow)
                else if (c == 'C') {
                    ccPos = Vec2(numCol, numRow)
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
            Console.err.println("distances="+dists.mkString)
            val minOne = dists.min
            val idxMin = dists indexOf minOne
            chosen = dirs(idxMin)
        }
        if (phase == PHASE_LOOK_FOR_CC && dirs.size >= 3) {
            // Exploration
            Console.err.println("Explore !")
            allPaths += currentPath
            val newPath = Path(Some(kirkPos), LinkedHashSet(), ListBuffer())
            currentPath.to += newPath
            
            val availablePaths=ListBuffer[Path]()
            if (lastPos isDefined) {
                // Create new path
                availablePaths ++= (for (d <- dirs 
                    if (d._2 != lastPos.get)
                    if !isPathKnown(kirkPos, d._2) )
                yield Path(Some(kirkPos), LinkedHashSet(d._2), ListBuffer()) )
                allPaths ++= availablePaths
            }
            // If kirk has already chosen one direction, stick to it
            val canKeepGoing = dirs filter (d=>d._1 == lastMove.get)
            if (canKeepGoing.size > 0) {
                // TODO: can simplify reusing _2 from dirs
                chosen = (lastMove.get, kirkPos + (deltaPos(lastMove.get)) )
            } else {
                // Determine another way, because we face a wall
                chosen = dirs.head
            }
            // Switch current path to chosen one
            Console.err.println("chosen="+chosen)
            Console.err.println("availablePaths="+availablePaths.mkString("\n"))
            currentPath = (availablePaths find (p=>p.nodes.head == chosen._2)).get
        }
        Console.err.println("currentPath="+currentPath)
        Console.err.println("allPaths="+allPaths.mkString("\n"))
        
        if (phase == PHASE_BACK_TO_START) {
            // Follow the path reverse
            path = path dropRight 1
            val desiredLoc = path.last
            val delta = desiredLoc - kirkPos
            chosen = (dirFromVec(delta), desiredLoc)
            
        } else if (dirs.size == 1 && lastPos.isDefined) {
            // Kirk is in a deadend => cut current path from the tree
            Console.err.println("back to path start")
            phase = PHASE_REWIND_PATH
            currentPath.open = false
            backPath = currentPath.nodes.toList.reverse
        }
        if (phase == PHASE_REWIND_PATH) {
            val desiredLoc =
                if (backPath.size == 1) {
                    phase = PHASE_LOOK_FOR_CC
                    backPath.head
                } else {
                    val next = backPath.head
                    backPath = backPath drop 1
                    next
                }
            Console.err.println("Back to "+desiredLoc)
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
        lastPos = Some(kirkPos)
        println(chosen._1) // Kirk's next move (UP DOWN LEFT or RIGHT).
    }
}