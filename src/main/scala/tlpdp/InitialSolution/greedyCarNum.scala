package tlpdp.InitialSolution

import tlpdp.common.Common
import tlpdp.lns.insert.InsertGreedy
import tlpdp.lns.remove.AllRemove
import tlpdp.model.{Request, Solution, Vehicle}
import tlpdp.pdp.Datas

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by hc on 17-2-25.
  */
class greedyCarNum (data : Datas) extends InitialSolution(data) {

  /*---------------------------模擬退火參數----------------------*/
  val maxIte : Int = 1000
  var nowIte : Int = 0
  val t0 : Double = 100           //初始溫度
  var t : Double = t0              //每次降火的溫度
  var mapkobList : Int =100       //Mapkob鏈
  val dt : Double = 0.95            //降火速度
  /*------------------------------------------------*/
  //最好解
  var best : Solution = new Solution()
  //當前最優解
  var curr : Solution = best
  //當前解
  var next : Solution = best

  val init = new InitialSolution(data)

  def greedyInitSolution(): Solution ={
    for(v <- vehicles)  v.reset()
    greedySolution(requests, vehicles)
  }
  /**
    * 將需求依次插入vehicle中，一輛車插滿再增加一輛車
    */
  def greedySolution(requests :  Array[Request], veh : Array[Vehicle]) : Solution = {
    val requestBank : ArrayBuffer[Request] = new ArrayBuffer[Request](0)    //未服務的request
    var cost : Double = 0.0    //總的費用，包括distance+time
//    val requestVolume = requests.sortWith(_.totalVolume() > _.totalVolume())            //將request按從箱子體積從大到小順序排列
    val rd  = Random.shuffle(0 to requests.length-1).toArray[Int]              //將request隨機排列
    for(ite <- 0 to requests.length-1){
      var carOrder : Int = 0    //從第一輪車開始
      val req = requests(rd(ite))
      var flag : Boolean = false
      while(carOrder<veh.length && !flag){
        val pair : insertPair = greedyOneRoute(req, veh(carOrder))        //貪心時都只比較距離，沒有包括時間
        if(pair.cost == Double.PositiveInfinity){
          carOrder += 1
        }else{
          flag = true
          addRequest(veh(pair.id), req, pair.pi, pair.di)
          veh(pair.id).distance += pair.cost
          //      distance += oneRequestPair(0).cost
        }
      }
      if(carOrder == veh.length){
        requestBank += req
      }
    }
    cost = calDistance(veh)
    val totalCost = calCostAddMissRequest(M, cost, requestBank.length)
    val init : Solution = new Solution(veh,requestBank, totalCost)
    println(init)
    init
  }

  /**
    * 一條路徑中的最佳位置
    */
  def greedyOneRoute(request : Request, vehicle: Vehicle) : insertPair = {

    var oneRoutePair : Array[insertPair] = new Array[insertPair](0)
    var bestPair : ArrayBuffer[insertPair] = new ArrayBuffer[insertPair]()
    var curr_cost : Double = Double.PositiveInfinity                             //去除distance比目前獲得的最短路徑還要長的解，減少packing次數
    for (i <- 1 to vehicle.customers(0) + 1){
      oneRoutePair = (
        for (j <- i+1 to vehicle.customers(0) + 2) yield {
          val costNotConstraint = addCostNotConstraint(vehicle,request,i,j)
          var cost:Double = Double.PositiveInfinity
            if(Common.less(costNotConstraint,curr_cost)){
              cost = addCost(vehicle,request,i,j,costNotConstraint)
              curr_cost = cost
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

  /**
    * 最小化路徑
    */
  def minRoutes(iniSol : Solution):Solution={
    var carNum : Int = calCarNum(iniSol)
    curr = iniSol.copySolution()
    best = iniSol.copySolution()
    val ar = new AllRemove(init)
    val ig = new InsertGreedy(init)
    while(nowIte < maxIte){
      for(i <- 0 to mapkobList){
        val randOrder : Int = Common.rand.nextInt(carNum)          //隨機選擇一條路徑移除所有乘客
        val removeSol = ar.allRemove(randOrder,curr,carNum)
        next = ig.greedyInsert(removeSol.bank.toArray,removeSol,carNum-1)
        if(accept(next)) {
          curr = next.copySolution()
          if(calCarNum(curr) < carNum && curr.bank.isEmpty) {
            best = curr.copySolution()
            carNum = calCarNum(curr)
          }
/*          if (Common.less(newObject(curr), newObject(best))) {
            best = curr.copySolution()
          }
          */
        }
        nowIte += 1
      }
      curr = greedyInitSolution().copySolution()
      carNum = calCarNum(curr)
    }
    best
  }

  /**
    * 新目標函數，帶權重車輛數目及距離
    * @param sol
    * @return
    */
  def newObject(sol: Solution):Double = {
    val a : Int = 500      //car weight
    val b : Int = 1       //car cost
    a*sol.vehicles.length + b*sol.cost
  }

  /**
    * accept準則，模擬退火淬火原則
    */
  def accept(sol : Solution) : Boolean= {
    if(sol.bank.isEmpty) true
//    else if(Common.less(newObject(sol), newObject(curr))) true
    else false
    /*
    else{
      val dif : Double = newObject(sol) - newObject(curr)
      if(Common.rand.nextDouble() < math.pow(math.E,dif/t)) true else false
    }
    */
  }
}
