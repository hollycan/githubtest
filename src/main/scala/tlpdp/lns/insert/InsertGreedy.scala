package tlpdp.lns.insert

import tlpdp.InitialSolution.InitialSolution
import tlpdp.common.Common
import tlpdp.model.{Request, Solution, Vehicle}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-1-8.
  */
class InsertGreedy(
//  inSolution : Solution,                  //傳入解，包括requestbank
  init : InitialSolution                  //過度類，獲取cost及addcost等方法
)extends Insert(init){


  override def insert(request: Array[Request] , sol : Solution, carnum:Int) : Solution = {
    greedyInsert(request,sol,carnum)
  }
/*
  /**
    * 貪心插入
    * @param request set of missing requests
    * @param sol current solution
    * @return
    */

  def greedyInsert(request: Array[Request], sol : Solution, carnum:Int) : Solution = {
    val missRequest : ArrayBuffer[Request] = new ArrayBuffer[Request](0)    //未服務的request
    missRequest ++= request
    var numMiss : Int = 0                           //未能被服務的request數
    var requestList : ArrayBuffer[insertPair] = new ArrayBuffer[insertPair]()        //存放可以被服務的request
    while(missRequest.length != 0 && numMiss != missRequest.length){
      var min_cost : Double = Double.PositiveInfinity                   //最小的插入值
      requestList.clear()
      numMiss = 0
      for(req <-0 until missRequest.length){
        var oneRequestPair : Array[insertPair] = (
          for(i <- 0 until sol.vehicles.length) yield {
 //           for(i <- 0 until carnum) yield {
            val pair : insertPair = greedyOneRoute(missRequest(req), req, sol.vehicles(i))               //獲取一條路徑中的最佳位置
            pair
          }).toArray
        oneRequestPair = oneRequestPair.sorted
        if(oneRequestPair(0).cost == Double.PositiveInfinity) numMiss += 1
        else if(Common.less(oneRequestPair(0).cost,min_cost)){
          min_cost = oneRequestPair(0).cost
          requestList += oneRequestPair(0)
        }
      }
      if(numMiss < missRequest.length){
        requestList = requestList.sortWith(_.rid > _.rid)                //按照rid從後往前刪除
        distincRequest(requestList, sol.vehicles.length)                                        //添加的request不能屬於同一輛車,否則會導致cost出錯
        for(r <- requestList){
          init.addRequest(sol.vehicles(r.vid), missRequest(r.rid), r.pi, r.di)
          sol.vehicles(r.vid).distance += r.cost
          sol.bank.remove(r.rid)
          missRequest.remove(r.rid)
        }
      }
    }
//    sol.cost = init.calCost(sol.vehicles)               //包括時間從窗的計算
    sol.cost = init.calDistance(sol.vehicles)
    val totalCost = init.calCostAddMissRequest(init.M, sol.cost, missRequest.length)
    val next : Solution = new Solution(sol.vehicles, missRequest, totalCost)
    next
  }

*/
  def distincRequest(rq : ArrayBuffer[insertPair], num : Int) :  Unit= {
    val vehicleFlag = new Array[Boolean](num).map(_ => false)       //對需要刪除的request做標志
    val rqFlag = new Array[Boolean](rq.length).map(_ => false)
    for(r <- 0 until rq.length){
      if(!vehicleFlag(rq(r).vid)) vehicleFlag(rq(r).vid) = true
      else{
        rqFlag(r) = true
      }
    }
    for(r <-rq.length-1 to 0 by -1 ){
      if(rqFlag(r)) rq.remove(r)
    }
  }


  def greedyInsert(request: Array[Request], sol : Solution, carnum:Int) : Solution = {
//    val g1 = System.currentTimeMillis()
    val missRequest : ArrayBuffer[Request] = new ArrayBuffer[Request](0)    //未服務的request
    missRequest ++= request
    var numMiss : Int = 0                           //未能被服務的request數
    var requestList : ArrayBuffer[insertPair] = new ArrayBuffer[insertPair]()        //存放可以被服務的request
    while(missRequest.length != 0 && numMiss != missRequest.length){
      var min_cost : Double = Double.PositiveInfinity                   //最小的插入值
      requestList.clear()
      numMiss = 0
      for(req <-0 until missRequest.length){
        var bestPairAllRoute : ArrayBuffer[insertPair] = new ArrayBuffer[insertPair]()           //每條路徑中最好的位置
        var oneRequestPair : Array[ArrayBuffer[insertPair]] = (
          for(i <- 0 until sol.vehicles.length) yield {
         // for(i <- 0 until carnum) yield {
            val pair : ArrayBuffer[insertPair] = greedyOneRoute(missRequest(req), req, sol.vehicles(i),i)               //獲取一條路徑中的所有插入位置
            if(pair.length >0) bestPairAllRoute += pair(0)
            pair
          }).toArray
          var flag = true
          while(flag && bestPairAllRoute.length > 0){
            bestPairAllRoute = bestPairAllRoute.sorted
            val p = bestPairAllRoute(0)
            if(p.cost == Double.PositiveInfinity)
              {
                numMiss += 1
                flag = false
              }
            else{

              if(init.feasibleLoading(sol.vehicles(p.vid),missRequest(req),p.pi,p.di)) {
                if (Common.less(p.cost, min_cost)) {
                  min_cost = p.cost
                  requestList += p
                }
                flag = false
              }
              else{
                bestPairAllRoute.remove(0)
                oneRequestPair(p.vid).remove(0)
                if(oneRequestPair(p.vid).length > 0)
                  bestPairAllRoute += oneRequestPair(p.vid)(0)
              }
            }

          }
          if(flag)  numMiss += 1

      }
      if(numMiss < missRequest.length){
        requestList = requestList.sortWith(_.rid > _.rid)                //按照rid從後往前刪除
        distincRequest(requestList, sol.vehicles.length)                                        //添加的request不能屬於同一輛車,否則會導致cost出錯
        for(r <- requestList){
          init.addRequest(sol.vehicles(r.vid), missRequest(r.rid), r.pi, r.di)
          sol.vehicles(r.vid).distance += r.cost
          sol.bank.remove(r.rid)
          missRequest.remove(r.rid)
        }
      }
    }
    //    sol.cost = init.calCost(sol.vehicles)               //包括時間從窗的計算
    sol.cost = init.calDistance(sol.vehicles)
    val totalCost = init.calCostAddMissRequest(init.M, sol.cost, missRequest.length)
    val next : Solution = new Solution(sol.vehicles, missRequest, totalCost)
//    println("g1 time: " + (System.currentTimeMillis() - g1)/1000.0+ "\n")      //s
    next
  }


}
