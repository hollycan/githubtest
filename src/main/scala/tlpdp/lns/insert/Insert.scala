package tlpdp.lns.insert

import tlpdp.InitialSolution.InitialSolution
import tlpdp.common.Common
import tlpdp.model.{Request, Solution, Vehicle}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-1-10.
  */
class Insert(
//  val inSolution : Solution,                  //傳入解，包括requestbank
  val init : InitialSolution                  //過度類，獲取cost及addcost等方法
)extends Serializable{

  var bank : ArrayBuffer[Request] = ArrayBuffer[Request]()        //未被服務的requests
  var now : Solution = new Solution()
  var missRequestNum : Int = 0                     //最開始未被服務的request的數量


  def insert(request: Array[Request] , sol : Solution, carnum:Int) : Solution = {
    sol
  }

  def setInit(inSolution : Solution) : Unit = {
    bank = inSolution.bank.clone()
    now = inSolution.copySolution()
    missRequestNum = now.bank.length
  }
  //用於存放request插入一條路徑後的最佳位置 vid - vehicle id ; cost 增加的費用(不包括時間窗） rid:request id(在missrequest需要刪除的id）
  class insertPair(val vid : Int, val rid : Int, val pi : Int, val di : Int, val cost : Double) extends Ordered[insertPair]{
    def compare(that : insertPair) : Int = {
      //按cost 順序
      val co = this.cost - that.cost
      if(co.abs <= 1e-6) 0
      else if (co > 0) 1
      else -1
    }
    def copyPair () : insertPair = {
      val pair = new insertPair(vid,rid,pi,di,cost)
      pair
    }
  }
  /**
    * 一條路徑中的最佳位置   (rid 是request在missrequest需要刪除的id）
    */
  /*
  def greedyOneRoute(request : Request, rid : Int, vehicle: Vehicle) : insertPair = {

    var oneRoutePair : Array[insertPair] = new Array[insertPair](0)
    var bestPair : ArrayBuffer[insertPair] = new ArrayBuffer[insertPair]()
    var curr_cost : Double = Double.PositiveInfinity
    for (i <- 1 to vehicle.customers(0) + 1){
      oneRoutePair = (
        for (j <- i+1 to vehicle.customers(0) + 2) yield {
          val costNotConstraint = init.addCostNotConstraint(vehicle,request,i,j)
          var cost:Double = Double.PositiveInfinity
          if(Common.less(costNotConstraint,curr_cost)){
            cost = init.addCost(vehicle,request,i,j,costNotConstraint)
            curr_cost = cost
          }
          val pair : insertPair = new insertPair(vehicle.id, rid, i, j, cost)
          pair
        }).toArray
      oneRoutePair = oneRoutePair.sorted
      bestPair += oneRoutePair(0)
    }
    bestPair = bestPair.sorted
    bestPair(0)
  }*/
// 用於三維
  def greedyOneRoute(request : Request, rid : Int, vehicle: Vehicle, vid: Int) : ArrayBuffer[insertPair] = {

    var oneRoutePair : Array[insertPair] = new Array[insertPair](0)
    var bestPair : ArrayBuffer[insertPair] = new ArrayBuffer[insertPair]()
    var curr_cost : Double = Double.PositiveInfinity
    for (i <- 1 to vehicle.customers(0) + 1){
      oneRoutePair = (
        for (j <- i+1 to vehicle.customers(0) + 2) yield {
//          val costNotConstraint = init.addCostNotConstraint(vehicle,request,i,j)
//          var cost:Double = Double.PositiveInfinity
//          if(Common.less(costNotConstraint,curr_cost)){
//            cost = init.addCostWithoutLoad(vehicle,request,i,j,costNotConstraint)
//            curr_cost = cost
//          }
          val costNotConstraint = init.addCostNotConstraint(vehicle,request,i,j)
          val cost = init.addCostWithoutLoad(vehicle,request,i,j,costNotConstraint)
          val pair : insertPair = new insertPair(vid, rid, i, j, cost)
          pair
        }).toArray
//      oneRoutePair = oneRoutePair.filter(_.cost != Double.PositiveInfinity)
      oneRoutePair = oneRoutePair.sorted
      bestPair ++= oneRoutePair
//      bestPair += oneRoutePair(0)
    }
    bestPair = bestPair.sorted
//    bestPair(0)
    bestPair
  }

 /*
  def greedyOneRoute(request : Request, rid : Int, vehicle: Vehicle) : insertPair = {

    var oneRoutePair : Array[insertPair] = new Array[insertPair](0)
    var bestPair : ArrayBuffer[insertPair] = new ArrayBuffer[insertPair]()
    var curr_cost : Double = Double.PositiveInfinity
    for (i <- 1 to vehicle.customers(0) + 1){
      oneRoutePair = (
        for (j <- i+1 to vehicle.customers(0) + 2) yield {
          val costNotConstraint = init.addCostNotConstraint(vehicle,request,i,j)
          var cost:Double = Double.PositiveInfinity
          if(Common.less(costNotConstraint,curr_cost)){
            cost = init.addCostWithoutLoad(vehicle,request,i,j,costNotConstraint)
            curr_cost = cost
                    }
          val pair : insertPair = new insertPair(vehicle.id, rid, i, j, cost)
          pair
        }).toArray

      oneRoutePair = oneRoutePair.sorted
   //   bestPair ++= oneRoutePair
       bestPair += oneRoutePair(0)
    }
    bestPair = bestPair.sorted
    bestPair(0)

  }*/

}
