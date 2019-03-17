import math._
import scala.util._
import scala.collection.mutable.ListMap

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
    val Array(w, h) = for(i <- readLine split " ") yield i.toInt
    
    val imgBytes = Array.ofDim[Int](h, w)
    val compressedImage = readLine split " "
    
    val lignesPortee = Array.ofDim[Int](5)
    
    var offset = 0
     for (i <- 0 until compressedImage.length/2 ) {
         val color = if (compressedImage(i*2) == "W") 0 else 1
         val nb = compressedImage(i*2+1).toInt
         for (j <-0 until nb) {
             imgBytes(offset / w)(offset % w) = color
             offset = offset + 1
         }
     }
     Console.err.println("offset="+offset+" w*h="+w*h);
      
    // 1) on va sommer les pixels noirs sur chaque ligne
    var max = 0
    var sumByLines = Array.ofDim[Int](h)
    for (y <- 0 until h) {
        // Sum of the whole line
        var sum = imgBytes(y).foldLeft(0)(_+_)
        sumByLines(y) = sum
        max = Math.max(sum, max)
    }
    Console.err.println("max="+max)
    
    //Console.err.println("sumByLines="+sumByLines.mkString(","))
    
    var thick = 1
    
    // Look for "01...10" sequence and return first "1" position
    // Maybe optimized with each line stored as String instead of byte[]
    def horizontalRay(y:Int):Seq[Int]={
        for (x <-1 until w-thick
            if imgBytes(y)(x-1)  == 0 &&
                foundThickTail(y, x, thick) &&
                // To avoid a false detection with half notes
                verticalRay(x)._2 > lineHeight)
        yield x
    }
    
    def foundThickTail(y:Int,x:Int,thick:Int)={
        var res = true
        for (i<-0 until thick) {
            res = res & imgBytes(y)(x+i) == 1
        }
        res & imgBytes(y)(x+thick) == 0
    }
    
    // Look for any pixel on a row, excluding stave lines
    // Return a tuple (middle, number of points)
    def verticalRay(x:Int):(Int, Int)={
        var sum=0 // Nb of black pixels
        var min=h
        var max=0
        for (y <-0 until h
            if !maxLines.contains(y) && imgBytes(y)(x) == 1) {
                sum=sum+1
                min = Math.min(min, y)
                max = Math.max(max, y)
            }
        if (sum == 0) (-1, -1) else ((max + min)/2, sum)
    }

    // Return every pixels ON on a row (to detect staves)    
    def countVerticalPixels(x:Int):Seq[Int]={
        for (y <-0 until h
            if imgBytes(y)(x) == 1) yield y
    }
    
    def ordonneeToNote(y:Int)={
        Console.err.println("ordonneeToNote en "+y)
        val res = (linesY(4) + lineHeight - y) / (lineHeight / 2.0)
        //Console.err.println("resultat="+res.toInt)
        Math.min(11, Math.round(res).toInt)
    }
    
    // Ca ne marche pas les sumByLines car les blanches coupent les lignes
    // de la portée. Du coup on fait autrement
    var foundStaves=false
    var x=0
    var maxLines:Seq[Int]=Seq()
    while (!foundStaves) {
        maxLines = countVerticalPixels(x)
        if (maxLines.size > 0)
            foundStaves = true
        x=x+1
    }
    
    // Maintenant, max=nb de pixels noir sur une ligne de portée
    // 2) on détermine l'ordonnée de chacune des lignes de la portée
    // en tenant compte de l'épaisseur des traits qui est variable
    val thickLine = maxLines.length / 5
    val linesY = for (i<-0 until 5) yield maxLines(i*thickLine)
    val lineHeight = linesY(1) - linesY(0)

    // On ajoute la ligne qui se rajoutent sur le DO du bas
    for (i <-0 until thickLine)
        maxLines = maxLines :+ linesY(4) + lineHeight + i
        
    Console.err.println("maxLines="+maxLines.mkString(","))
    Console.err.println("linesY="+linesY.mkString(","))
    Console.err.println("lineHeight="+lineHeight)
    
    // 3) on envoie un rayon au milieu de chaque ligne
    val overLinesY = (linesY(0) - lineHeight) +: linesY :+ (linesY(4) + lineHeight)
            
    val notes="CDEFGABCDEFG"
    val decodedNotes=scala.collection.mutable.Map[Int,String]()
    for (iterationThick <-1 to 4 if decodedNotes.isEmpty) {
        Console.err.println("On teste avec thick="+iterationThick)
        thick = iterationThick
        for (i <-0 until 7) {
            var yy = overLinesY(i) + lineHeight/2
            var rh = horizontalRay(yy)
            if (rh.size > 0) {
                Console.err.println("Rayon en "+yy) //+" = "+ rh)
                for (x <-rh) {
                    for (tryX <- Seq(x-1, x+thick)) {
                        val result = verticalRay(tryX)
                        if (result._1 != -1 && decodedNotes.get(x).isEmpty) {
                            Console.err.println("Pixels sur la colonne "+x+"-"+thick+"="+result)
                            val note = notes(ordonneeToNote(result._1))
                            // Determine if note is quarter or half
                            val noteMiddleX = if (trayX == x-1) x - lineHeight/2 else x+lineHeight/2
                            val blackWhite = imgBytes(result._1)(noteMiddleX) == 0
                            
                            val duration = if (blackWhite) "H" else "Q"
                            decodedNotes += (x -> (note + duration))
                            Console.err.println("Note ="+note+blackWhite)
                        }
                    }
                }
            }
        }
    }
    // 4) on trie les notes dans l'ordre des abscisses
    Console.err.println(decodedNotes)
    val orderedNotes = decodedNotes.toSeq.sortBy(_._1) map { case(k,v) => v }

    println(orderedNotes.mkString(" "))
}