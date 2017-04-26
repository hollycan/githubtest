package tlpdp.model

import tlpdp.InitialSolution.{InitialSolution, greedy}
import tlpdp.common.Common

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-3-5.
  */
class SolutionPool(
    val init : InitialSolution,
    difference : Double = 0.1             //最小允許的於當前最好解的不同程度
  ) extends Serializable{

  var solutionSet : ArrayBuffer[Solution] = new ArrayBuffer[Solution]()
  var best : Solution = new Solution()
  best.cost = Double.PositiveInfinity

  /**
    * 將分塊過程獲得的新解加入解集合中，如果重復，則不加入
    * @param sol
    */
  def add(sol : Solution) : Unit = {
    if(!solutionSet.contains(sol)){
      sol.improvedFlag = true
      if(!solutionSet.contains(sol)) {
        sol.improvedFlag = false
        solutionSet += sol
        if(Common.less(sol.cost,best.cost)) best = sol.copySolution()
      }
    }
  }

  /**
    * 從解集中選取初始解
    */
  def startSolution():Solution = {
    solutionSet = solutionSet.sortWith(_.cost < _.cost)
    if(!best.improvedFlag){
      best.improvedFlag = true
      return best
    }
    else{
      for(s <- solutionSet if !s.improvedFlag){
        if(diff(s,best))  return s
      }
    }
    val restart = new greedy(init.data)
    restart.greedyInitSolution()
  }

  /**
    * 獲取當前解於最好解不同的邊數
    * @param sol
    * @param best
    * @return
    */
  def diff(sol: Solution, best: Solution) : Boolean ={
    val edgeUse = init.costs.map(l => l.map(_=>1))
    var diffEdge :Double= 0
    var totalEdge : Double= 0
    for(c <- best.vehicles if c.customers(0) !=0){
      edgeUse(0)(c.customers(1)) =0
      totalEdge += 1
      for(i <- 1 until c.customers(0)){
        edgeUse(c.customers(i))(c.customers(i+1)) = 0
        totalEdge += 1
      }
      totalEdge += 1
      edgeUse(c.customers(c.customers(0)))(0) = 0
    }
    for(s <- sol.vehicles if s.customers(0)!=0){
      diffEdge = diffEdge + edgeUse(0)(s.customers(1))
      for(j <- 1 until s.customers(0))
        diffEdge = diffEdge + edgeUse(s.customers(j))(s.customers(j+1))
      diffEdge = diffEdge + edgeUse(s.customers(s.customers(0)))(0)
    }
    if(Common.more(diffEdge/totalEdge, difference)) true else false
  }

}
