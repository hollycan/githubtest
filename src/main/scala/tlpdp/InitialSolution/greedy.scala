package tlpdp.InitialSolution

import tlpdp.common.Common
import tlpdp.model.{Request, Solution, Vehicle}
import tlpdp.pdp.Datas

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
/**
  * Created by hc on 17-1-4.
  */
class greedy(data : Datas) extends InitialSolution(data) {

  def greedyInitSolution(): Solution ={
    for(v <- vehicles)  v.reset()
    greedySolution(requests, vehicles)
  }

  def greedyMultInitSolution(numPartitions : Int):Seq[Solution]= {
    val iniSols = for(i<- 0 until numPartitions) yield {
      greedyInitSolution()
    }
    iniSols
  }
  /**
    * 將需求貪心插入vehicle中
    */
  def greedySolution(requests :  Array[Request], veh : Array[Vehicle]) : Solution = {

    val requestBank : ArrayBuffer[Request] = new ArrayBuffer[Request](0)    //未服務的request
    var cost : Double = 0.0    //總的費用，包括distance+time
//    val requestVolume = requests.sortWith(_.totalVolume() > _.totalVolume())            //將request按從箱子體積從大到小順序排列
    val requestVolume = requests.sortWith(_.boxesWeight > _.boxesWeight)            //目前效果最好
//    val requestWeight = requests.sortWith(_.boxesWeight > _.boxesWeight)           //將request按weight從大到小排列,用於一維的情況
//      val requestDistance = requestVolume.sortWith(_.totalDistance() < _.totalDistance())
    //    val rd  = Random.shuffle(0 to requests.length-1).toArray[Int]              //將request隨機排列
//    for(ite <- 0 to requests.length-1){
//      val req = requests(rd(ite))
    for(req <- requestVolume){
      var curr_cost : Double = Double.PositiveInfinity
      var oneRequestPair : Array[insertPair] = (
        for(i <- 0 until veh.length) yield {
          val pair : insertPair = greedyOneRoute(req, veh(i),curr_cost)        //貪心時都只比較距離，沒有包括時間，獲取一條路徑最佳位置
          if(Common.less(pair.cost,curr_cost))  curr_cost = pair.cost          //curr_cost爲最小插入代價，後邊的計算費用大於curr_cost都可忽略
          pair
        }
      ).toArray
      oneRequestPair = oneRequestPair.sorted
      if(oneRequestPair(0).cost == Double.PositiveInfinity){
        requestBank += req
      }else{
        addRequest(veh(oneRequestPair(0).id), req, oneRequestPair(0).pi, oneRequestPair(0).di)
        veh(oneRequestPair(0).id).distance += oneRequestPair(0).cost
      }
    }
//    cost = calCost(veh)
    cost = calDistance(veh)
    val totalCost = calCostAddMissRequest(M, cost, requestBank.length)
    val init : Solution = new Solution(veh,requestBank, totalCost)
    println(init)
    init
  }

  /**
    * 一條路徑中的最佳位置
    */
  def greedyOneRoute(request : Request, vehicle: Vehicle,min_cost:Double) : insertPair = {

      var oneRoutePair : Array[insertPair] = new Array[insertPair](0)
      var bestPair : ArrayBuffer[insertPair] = new ArrayBuffer[insertPair]()
      var curr_cost : Double = Double.PositiveInfinity                             //去除distance比目前獲得的最短路徑還要長的解，減少packing次數
      for (i <- 1 to vehicle.customers(0) + 1){
        oneRoutePair = (
        for (j <- i+1 to vehicle.customers(0) + 2) yield {
          val costNotConstraint = addCostNotConstraint(vehicle,request,i,j)
          var cost:Double = Double.PositiveInfinity
          if(Common.less(costNotConstraint,min_cost)){                     //min_cost 爲之前最好的解
            if(Common.less(costNotConstraint,curr_cost)){                  //curr_cost爲該條路徑中獲得的最好解
              cost = addCost(vehicle,request,i,j,costNotConstraint)
              curr_cost = cost
            }
          }
          val pair : insertPair = new insertPair(vehicle.id, i, j, cost)
          pair
        }).toArray
        oneRoutePair = oneRoutePair.sorted
        bestPair += oneRoutePair(0)
      }
      bestPair = bestPair.sorted
      bestPair(0)
  }


}

object greedy{
  def main(args : Array[String]):Unit= {
    val data = new Datas("3L-PDP-instances/050_CLUS_2_1.txt")
    data.read3LPDP()
    val gr = new greedy(data)
    println("----------------init greedy start-----------------")
    gr.greedyInitSolution()
    println("----------------init greedy end-------------------")
  }
}
