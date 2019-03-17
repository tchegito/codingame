import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
    val nbOods = readInt
    val price = readInt
    var budgets = Array.ofDim[Int](nbOods)
    for(i <- 0 until nbOods)
        budgets(i) = readInt
    
    Console.err.println("Nombre de oods:"+nbOods)
    Console.err.println("Prix:"+price)
    budgets foreach ( a=>Console.err.println(a) )
    
    // 1) calculate the MAX sum
    val max = budgets.sum
    if (max < price)
        println("IMPOSSIBLE")
    else {
        var delta = max - price
        // On doit enlever 'delta' réparti sur l'ensemble des budgets
        
        var valPresentes = scala.collection.mutable.Map[Int, Int]()
        for (i <- 0 until nbOods) {
            var nb = valPresentes.getOrElse(budgets(i), 0)
            valPresentes(budgets(i)) = nb+1
        }
        while (delta > 0) {
            // 1) on enlève les doublons
            //Console.err.println("Apres iteration:")
            //budgets foreach ( a=>Console.err.print(a+",") )
            Console.err.println()
            
            Console.err.println("On a delta="+delta)
            // On cherche un nombre qui peut absorber delta à lui tout seul
            
            // On retranche 1 au max
            if (delta > 0) {
                val m = budgets.max
                val idx = budgets.indexOf(m)
                budgets(idx) = budgets(idx) - 1
                //valPresentes(m) = valPresentes(m) -1
                //valPresentes(m-1) = valPresentes.getOrElse(m-1, 0) +1
                delta = delta - 1
            }

            // Cas particulier: (optim de la solution pour baisser la contrib max)
            // on a un nb à zéro et un max unique
            /*
            if (delta == 0) {
                var success=true
                while (success) {
                    success = false
                    val m = budgets.max
                    val idxZero = budgets.indexOf(0)
                    if (idxZero != -1 && valPresentes(m) == 1) {
                        budgets(idxZero) = budgets(idxZero) + 1 // = 0
                        val idxMax = budgets.indexOf(m)
                        budgets(idxMax) = budgets(idxMax) - 1 
                        success = true
                    }
                }
            }
            */
            // On recherche la solution optimale
            val plusGrandeContribution = budgets.max
            /*
            for (i <- 0 until nbOods) {
                if (budgets(i) > delta) {
                    val newVal = budgets(i) - delta
                    if (valPresentes(newVal)) {
                        // La valeur existe déjà => on ne peut pas enlever tout le delta
                        var j = delta
                        while (valPresentes(newVal)) {
                            newVal--
                        }
                    }
                    delta = 0;
                }
            }
            */
            
        }
        // Return solution
        
        budgets.sortWith(_<_) foreach ( a=>println(a) )
    

    }
}