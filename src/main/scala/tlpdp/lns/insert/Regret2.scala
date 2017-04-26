package tlpdp.lns.insert

import tlpdp.InitialSolution.InitialSolution
import tlpdp.common.Common
import tlpdp.model.{Request, Solution}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-1-10.
  */
class Regret2 (
//  inSolution : Solution,                  //傳入解，包括requestbank
  init : InitialSolution                  //過度類，獲取cost及addcost等方法
)extends Insert(init){

  override def insert(request: Array[Request] , sol : Solution, carnum : Int) : Solution = {
    regretTwoInsert(request,sol,carnum)
  }


  /**
    * regret2插入方法，每次選取頭兩個差異最大的request
    * @param request
    * @param sol
    * @return
    */
 /*
  def regretTwoInsert (request: Array[Request], sol : Solution,carnum: Int) : Solution = {
    val missRequest : ArrayBuffer[Request] = new ArrayBuffer[Request](0)    //未服務的request
    var preInsert : insertPair = new insertPair(0,0,0,0,0.0)                //存儲當前差距最大的request
    missRequest ++= request
    var numMiss : Int = 0                           //未能被服務的request數
    while(missRequest.length != 0 && numMiss != missRequest.length){
      var max_cost : Double = 0.0                   //最大相差費用
      numMiss = 0
      for(req <-0 until missRequest.length) {
        var oneRequestPair : Array[insertPair] = (
          for(i <- 0 until sol.vehicles.length) yield {
 //           for(i <- 0 until carnum) yield {
            val pair : insertPair = greedyOneRoute(missRequest(req), req, sol.vehicles(i))
            pair
          }).toArray
        oneRequestPair = oneRequestPair.sorted
        var difference : Double = 0.0
        if(oneRequestPair(0).cost== Double.PositiveInfinity) numMiss += 1
        else if(oneRequestPair(1).cost == Double.PositiveInfinity){
          difference = oneRequestPair(0).cost
        }else{
          difference = oneRequestPair(1).cost - oneRequestPair(0).cost
        }
        if(Common.more(difference,max_cost) || !(oneRequestPair(0).cost == Double.PositiveInfinity)){
          max_cost = difference
          preInsert = oneRequestPair(0).copyPair()
        }
      }
      if(numMiss < missRequest.length){
          init.addRequest(sol.vehicles(preInsert.vid), missRequest(preInsert.rid), preInsert.pi, preInsert.di)
          sol.vehicles(preInsert.vid).distance += preInsert.cost
          missRequest.remove(preInsert.rid)
          sol.bank.remove(preInsert.rid)
      }
    }
//    sol.cost = init.calCost(sol.vehicles)
    sol.cost = init.calDistance(sol.vehicles)
    val totalCost = init.calCostAddMissRequest(init.M, sol.cost, missRequest.length)
    val next : Solution = new Solution(sol.vehicles, missRequest, totalCost)
    next
  }
*/


 def regretTwoInsert (request: Array[Request], sol : Solution,carnum: Int) : Solution = {
//   val g2 = System.currentTimeMillis()
   val missRequest : ArrayBuffer[Request] = new ArrayBuffer[Request](0)    //未服務的request
   var preInsert : insertPair = new insertPair(0,0,0,0,0.0)                //存儲當前差距最大的request
   missRequest ++= request
   var numMiss : Int = 0                           //未能被服務的request數
   while(missRequest.length != 0 && numMiss != missRequest.length){
     var max_cost : Double = -1.0                   //最大相差費用
     numMiss = 0
     for(req <-0 until missRequest.length) {
       var bestPairAllRoute : ArrayBuffer[insertPair] = new ArrayBuffer[insertPair]()
       var oneRequestPair : Array[ArrayBuffer[insertPair]] = (
         for(i <- 0 until sol.vehicles.length) yield {
           //   for(i <- 0 until carnum) yield {
           var pair : ArrayBuffer[insertPair] = greedyOneRoute(missRequest(req), req, sol.vehicles(i),i)
           if(pair.length >0) bestPairAllRoute += pair(0)
           pair
         }).toArray
       var flag = true
       while(flag && bestPairAllRoute.length>0){
         bestPairAllRoute = bestPairAllRoute.sorted
         var difference : Double = 0.0
         //          println(bestPairAllRoute.length)
         if(bestPairAllRoute(0).cost== Double.PositiveInfinity){
           numMiss += 1
           flag =false
         }else if(bestPairAllRoute.length<2 || bestPairAllRoute(1).cost == Double.PositiveInfinity) {
           val p  = bestPairAllRoute(0)
           if(init.feasibleLoading(sol.vehicles(p.vid),missRequest(req),p.pi,p.di)) {
             difference = p.cost
             if(Common.more(difference,max_cost)){
               max_cost = difference
               preInsert = p.copyPair()
             }
             flag = false
           }else {
             bestPairAllRoute.remove(0)
             oneRequestPair(p.vid).remove(0)
             if(oneRequestPair(p.vid).length > 0)
               bestPairAllRoute += oneRequestPair(p.vid)(0)
           }
         }else {
           val p1 = bestPairAllRoute(0)
           val p2 = bestPairAllRoute(1)
           if(init.feasibleLoading(sol.vehicles(p1.vid),missRequest(req),p1.pi,p1.di))
             if(init.feasibleLoading(sol.vehicles(p2.vid),missRequest(req),p2.pi,p2.di)) {
               difference = p2.cost - p1.cost
               if(Common.more(difference,max_cost)){
                 max_cost = difference
                 preInsert = p1.copyPair()
               }
               flag = false
             }else{
               bestPairAllRoute.remove(1)
               oneRequestPair(p2.vid).remove(0)
               if(oneRequestPair(p2.vid).length > 0)
                 bestPairAllRoute += oneRequestPair(p2.vid)(0)
             }
           else{
             bestPairAllRoute.remove(0)
             oneRequestPair(p1.vid).remove(0)
             if(oneRequestPair(p1.vid).length > 0)
               bestPairAllRoute += oneRequestPair(p1.vid)(0)
           }
         }
       }
       if(flag)  numMiss += 1
     }
     if(numMiss < missRequest.length){
       init.addRequest(sol.vehicles(preInsert.vid), missRequest(preInsert.rid), preInsert.pi, preInsert.di)
       sol.vehicles(preInsert.vid).distance += preInsert.cost
       missRequest.remove(preInsert.rid)
       sol.bank.remove(preInsert.rid)
     }
   }
   //    sol.cost = init.calCost(sol.vehicles)
   sol.cost = init.calDistance(sol.vehicles)
   val totalCost = init.calCostAddMissRequest(init.M, sol.cost, missRequest.length)
   val next : Solution = new Solution(sol.vehicles, missRequest, totalCost)
//   println("g2 time: " + (System.currentTimeMillis() - g2)/1000.0+ "\n")      //s
   next
 }

}
