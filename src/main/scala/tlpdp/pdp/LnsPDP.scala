package tlpdp.pdp

import java.io.FileWriter

import tlpdp.InitialSolution.{InitialSolution, greedy, greedyCarNum}
import tlpdp.common.Common
import tlpdp.lns.insert.{Insert, InsertGreedy, Regret2, Regret3}
import tlpdp.lns.remove.{RandomRemove, Removal, ShawRemove, WorstRemove}
import tlpdp.loadFeasible.{BasicHeuristic, MultiLayerSearch, Packing}
import tlpdp.model._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-1-10.
  */
class LnsPDP(
    val init : InitialSolution,
    val numPartition : Int = 1,       //默認不分布式
    val index : Int   //節點編號
) extends Serializable{

  def this(data : Datas, numPartition : Int, index : Int) = {
    this(new InitialSolution(data), numPartition, index)
  }
  /*--------全局參數--------------*/
  //最大迭代次數
  var maxIte : Int = 500000
  //最大未提升的迭代次數
  val maxNoImp : Int = (init.requests.length * 1000)
  //禁忌表長
  var tabuListLen : Int = (init.requests.length * 0.4).toInt

  //禁忌表
  var tabuList : Array[TabuItem] = new Array[TabuItem](tabuListLen)
  //最近替換的禁忌表中位置
  var nowPosition : Int = 0
  //懲罰因子M
  val M : Double = 1000.0

  /*
   * 设置最大迭代次数
   */
  def setMaxIte(ite: Int): Unit = {
    this.maxIte = ite
  }
  def setTemp(t : Double , d : Double) : Unit = {
    this.t0 = t
    this.dt = d
  }
  def setTabuLen(num : Int):Unit = {
    this.tabuListLen = num
  }
  /*
   * 设置初始解
   */
  def setInitial(solution: => Solution): Unit = {

    curr = solution.copySolution()
    best = curr.copySolution()
  }

  /*--------ts運行期參數-----------*/
  //最好解
  var best : Solution = new Solution()
  //當前最優解
  var curr : Solution = best.copySolution()
  //當前解
  var next : Solution = best.copySolution()
  //當前迭代次數
  var nowIte : Int = 0
  //當前未提升的迭代次數
  var nowNoImp : Int = 0
  //獲得最好解的迭代次數
  var bestIte : Int = 0

  /*-------鄰域設置----------------*///shawremove中p爲1時爲random remove
  val removals : Array[Removal] = Array(new ShawRemove(init), new RandomRemove(init), new WorstRemove(init))
  //remove  權重
  var r1 : Double = 0.3
  var r2 : Double = 0.7
  var r3 : Double = 1.0
  val removeWeight : Array[Double] = Array(r1,r2,r3)
  def setRemoveWeight(rs1:Double,rs2:Double,rs3:Double):Unit= {
    this.r1 = rs1
    this.r2 = rs2
    this.r3 = rs3
  }
  val inserts : Array[Insert] = Array(new InsertGreedy(init), new Regret2(init), new Regret3(init))
  //insert 權重
  val insertWeight : Array[Double] = Array(0.3,0.8,1.0)

  /*-------模擬退火參數-------------*/
  var t0 : Double = 30           //初始溫度
  var t : Double = t0              //每次降火的溫度
  var mapkobList : Int = 150        //Mapkob鏈
  var dt : Double = 0.998            //降火速度

  var eTime = System.currentTimeMillis()    //獲取當前時間
  var sTime = System.currentTimeMillis()    //獲取當前時間

  var Maxvolume : Double = 0                //裝載過程獲得的最大體積

  /*---------remove  參數---------------*/
  var p : Int = 4             //當p爲1時，shawRemove爲random remove
  var low : Double = 0.04
  var high : Double = 0.2
  /*-------------------------------------參數設置結束----------------------------------------------*/
  var T : Double = 5.0        //時間限制
  def setT(t : Double):Unit = {
    this.T = t
  }
//  val outfile = new FileWriter("/home/hc/IdeaProjects/result/new/3L-pdp_75_5run_time", true)
//  outfile.write("------------------Routing RESULT-----------------\n")
//  outfile.close()

  def resetSolution():Unit={
    best  = new Solution()
    curr  = best.copySolution()
    next  = best.copySolution()
  }
  def resetParameter():Unit = {
    nowIte = 0
    nowNoImp = 0
    tabuList = new Array[TabuItem](tabuListLen)
    nowPosition = 0
    bestIte = 0
    Maxvolume = 0
    t = t0
  }
  /**
    * ts過程
    */
  def ts() : Unit = {
    resetParameter()
    sTime = System.currentTimeMillis()    //並行化時在此設置初始時間
    var timeIte : Int = 1   //1m print result

    var carnum : Int =0     //增加車輛數，使得之後優化的解的車輛數都比初始解小,不用於3L-PDP
//    carnum = init.calCarNum(curr)

    while(notStop()){
      for(i <- 0 until mapkobList) {
        if(Common.more((System.currentTimeMillis()- sTime)/60000.0, timeIte))
        {
//          val outfile = new FileWriter("/home/hc/IdeaProjects/result/new/3L-pdp_75_5run_time", true)
//          outfile.write("timeIte:"+timeIte + "cost:"+ best.cost + "\n")
          println("timeIte:"+timeIte + "cost:"+ best.cost)
          timeIte += 1
        }
        if(Common.more((System.currentTimeMillis()- sTime)/60000.0, T)) return
        val nextAndItem : SolutionItem = removeRequests(curr)        //刪除request
        val t2 = System.currentTimeMillis()
        val nextTemp = insertRequests(nextAndItem.sol,carnum)                //插入request
//        println("insertTime: " + (System.currentTimeMillis() - t2)/1000.0+ "\n")
        next = nextTemp.copySolution()
        val loadingFlag : Boolean = feasible3DLoading(next.vehicles)
//      var loadingFlag : Boolean = true
//        if(!loadingFlag) println("**************Insert Loading Error*********************")
        //因爲在插入request的時還，有n個request需求需要插入，在判斷負載約束的時候是單個request的判斷，所有即使單個request滿足約束的前提下，當同時添加多個request的時還，可能就不能夠滿足負載約束，所以在此處還需判斷一下整體解的負載約束
//        checkSolution(next)
//        println((System.currentTimeMillis()- sTime))
        if(accept(next)&&loadingFlag) {                 //如果裝載不可行也無法接受解
          curr = next.copySolution()
          if (Common.less(curr.cost, best.cost)) {
            best = curr.copySolution()
//           println("nowNoImp:"+nowNoImp + "cost:"+ best.cost)
//            println("total time: " + (System.currentTimeMillis() - sTime)/1000.0+ "\n")      //s
            nowNoImp = 0
            bestIte = nowIte
          } else {
            nowNoImp += 1
          }
        }
        setTabu(nextAndItem.items)
        nowIte += 1
        nowNoImp += 1
        if(nowNoImp % 150 == 0) high = 0.2 else high = 0.2
      }
      t = t * dt
    }
  }

  /**
    * stop criterion
    */
  def notStop() : Boolean = {
    nowIte < maxIte && nowNoImp < maxNoImp
  }

  /**
    * 添加tabu項
    */
  def setTabu(item : ArrayBuffer[TabuItem]) : Unit = {
    for(it <- item){
      if(!Common.isTabu(it, tabuList)){                             //防止臨界情況
        tabuList(nowPosition) = it
        nowPosition = (nowPosition + 1) % tabuListLen
      }
    }
  }
  /**
    * accept準則，模擬退火淬火原則
    */
   def accept(sol : Solution) : Boolean= {
     if(Common.less(sol.cost, curr.cost)) true
     else{
        val dif : Double = curr.cost - sol.cost
        if(Common.rand.nextDouble() < math.pow(math.E,dif/t)) true else false
     }
   }


  /**
    * 選擇remove優化，刪除request
    */
   def removeRequests(sol : Solution) : SolutionItem = {
     val removeHeurist = chooseRemoveOperator(Common.rand.nextDouble())
     var Rnum : Int = (low*init.customers.length).toInt + Common.rand.nextInt((high*init.customers.length).toInt)
     Rnum = 1
     removeHeurist.removal(Rnum, sol, p, tabuList)                 //若request在禁忌表中無法移除
   }

  /**
    * 輪盤賭選擇算子 remove
    */
   def chooseRemoveOperator (r : Double) : Removal = {
     for(i <- 0 until removals.length if r <= removeWeight(i) )
       return removals(i)
     return removals(0)

   }

  /**
    * 選擇insert優化 插入request
    */
   def insertRequests(sol : Solution, carnum:Int) : Solution = {
     val  insertHeurist = chooseInsertOperator(Common.rand.nextDouble())
     insertHeurist.insert(sol.bank.toArray, sol, carnum)
   }
  /**
    * 輪盤賭選擇算子 insert
    */
   def chooseInsertOperator (r : Double) : Insert = {
    for(i <- 0 until inserts.length if r <= insertWeight(i) )
      return inserts(i)
    return inserts(0)
  }


  /**
    * 三維裝載可行性,在insert之後檢驗
    * @param vehicle
    * @return
    */
  def feasible3DLoadingOneRoute(vehicle: Vehicle) : Boolean = {
    var origin : Position = new Position()    //創建原點坐標
    origin.lx = vehicle.length
    origin.ly = vehicle.width
    origin.lz = vehicle.height
    var boxes : ArrayBuffer[Box] = new ArrayBuffer[Box]()           //total boxes
    for(ite <- 1 to vehicle.customers(0)){
      if(true || !isPack(vehicle,ite)) {
        boxes.clear()
        for (r <- 1 to ite) {
          if (init.customers(vehicle.customers(r)).character == 0) {
            val outOrder = init.getOutOrder(vehicle.customers, r, vehicle.customers(0) + 1, vehicle.customers(0) + 1)
            boxes ++= init.getCustomerBox(init.getRequestId(vehicle.customers(r)), r, outOrder) //獲取pick的箱子並且賦予箱子pick與delivery的順序
          } else {
            init.removeBox(boxes, r)
          }
        }
        var vol: Double = 0
        for (b <- boxes) vol += b.boxVolume
        if (Common.more(vol, Maxvolume)) Maxvolume = vol
        val ps: Array[Int] = new Array[Int](boxes.length)
//        println("444444444444444444444444")
//        val bh = new BasicHeuristic(ps, origin, boxes)
        val bh = new MultiLayerSearch(origin,boxes)
//        val bh = new Packing(origin,boxes)
//        if (!bh.packing()) {
        if(!bh.basicHeuristic()){
//          println("ite:" + ite + " " + vehicle.toString)
          return false
        }
      }
    }
    true
  }

  /**
    * 加快檢查速度
    * @param vehicle
    * @param ite
    * @return
    */
  def isPack(vehicle: Vehicle,ite : Int): Boolean = {
    if(ite+1<=vehicle.customers(0)  && init.customers(vehicle.customers(ite+1)).character==0 )
      return true
    if(init.customers(vehicle.customers(ite)).character != 0 )
      return true
    false
  }

  def feasible3DLoading(vehicle: Array[Vehicle]) : Boolean = {
    for(v <- vehicle){
      if(!feasible3DLoadingOneRoute(v))
        return false
    }
    true
  }
  /**
    * 主體流程
    */
   def tabuLns() : Solution = {
    println("------------------Pogress start--------------------")
    println("------------------Generating initial solution--------------------")
    //××××××××××××××××××××××××××××××構造初始解×××××××××××××××××××××××××××××××××××××××
     resetSolution()
     curr.cost = Double.PositiveInfinity
     val initial = new greedy(init.data)

//     val initial = new greedyCarNum(init.data)             //以車輛數少的初始解貪心構造過程
     var minCarnum : Int = init.vehicleMaxNum +1             //用於搜索過程不會產生比初始解獲得車輛更多的解，3L-PDP中沒用到

     sTime = System.currentTimeMillis()    //獲取當前時間
     for(i<-1 to 1){
       val randCurr = initial.greedyInitSolution()
       println("init time: " + (System.currentTimeMillis() - sTime)/1000.0+ "\n")      //s
       if(Common.less(randCurr.cost,curr.cost))              //3L-PDP選擇距離最小的解
         {
             curr = randCurr.copySolution()
         }
     }
//     curr = initial.minRoutes(curr)                         //最小化路徑初始構造過程
     checkSolution(curr)                                //核對解的正確性,包括裝載可行性驗證
     best = curr.copySolution()
     next = curr.copySolution()
    //×××××××××××××××××××××××××××初始解構造結束×××××××××××××××××××××××××××××××××××××××××××

    println("-------Tabu start----------")
    //×××××××××××××××××××××××××××tabu過程×××××××××××××××××××××××××××××××××××××××
    ts()
    Maxvolume = 0
    checkSolution(best)
    println("Best result:")
    println("best Ite:" + bestIte + " best solution" + best)
    println("best Volume: " + Maxvolume)
//    outfile.close()
    best
  }

  /**
    * 核對解的正確性
    * @param sol
    * @return
    */
  def checkSolution(sol : Solution):Boolean = {
    var sumDistance = 0.0
    var sumCost = 0.0
    val cusSelected = new Array[Boolean](init.numCUs ).map(_ => false)       //判斷乘客是否都被選擇
    val reqSelected = new Array[Boolean](init.requests.length + 1).map(_=>false)         //判斷request是否被選擇
    sol.vehicles.foreach(v =>{
      var carDistance = 0.0
      if(v.customers(0) != 0){
        val cusNum = v.customers(0)
        carDistance = carDistance + init.costs(0)(v.customers(1))
        (1 to cusNum).foreach(i => carDistance = carDistance + init.costs(v.customers(i))(if (i < cusNum) v.customers(i+1) else 0))
        (1 to cusNum).foreach(i => cusSelected(v.customers(i)) = true)
        (0 until v.requests.length).foreach(i => reqSelected(v.requests(i).id) = true)
        if(!Common.eq(carDistance,v.distance)){
          println("*************Error : car cost(" + carDistance + ")***************" + v.toString)
          return false
        }

        if(Common.less(v.capacity, v.nowCapacity.max)) {
          println("***********Error : car capacity (" + v.nowCapacity.max + ")************" + v.toString)
          return false
        }
      }
      sumDistance += carDistance
    })
    if(cusSelected(0)){
      println("*********Error : customer 0 selected**********")
      return false
    }

    for(i <- 0 until sol.bank.length){
      if(reqSelected(sol.bank(i).id)){
        println("***********Error: request repeat***********:request: " + sol.bank(i))
        return false
      }else {
        reqSelected(sol.bank(i).id) = true
        if(cusSelected(sol.bank(i).pick.id)){
          println("***********Error: customer repeat***********:customer: " + sol.bank(i).pick.id)
          return false
        }else
          cusSelected(sol.bank(i).pick.id) = true
        if(cusSelected(sol.bank(i).delivery.id)){
          println("***********Error: customer repeat***********:customer: " + sol.bank(i).delivery.id)
          return false
        }else
          cusSelected(sol.bank(i).delivery.id) =true
      }
    }

    for(i <- 1 until init.numCUs){
      if(!cusSelected(i)){
        println("*************Error : customer missed**********: customer: " + i)
        return false
      }
    }

    for(i <- 1 to init.requests.length){
      if(!reqSelected(i)){
        println("*************Error : request missed**********: request: " + i)
        return false
      }
    }
//    sumCost = init.calCost(sol.vehicles)            暫時只考慮distance
    sumCost = sumDistance + M * sol.bank.length
    if(!Common.eq(sumCost, sol.cost)){
      println("**********Error : solution ****** real cost=" + sumCost + " now cost=" + sol.cost)
      return false
    }
    var loadingFlag : Boolean = feasible3DLoading(sol.vehicles)
    if(!loadingFlag){
      println("***************************loading error***********************")
      return false
    }

    println("**************** check successful ************************")
    true
  }
}

object LnsPDP {

  def main(args : Array[String]):Unit= {

    /*------------------pdp tes-------------------------------*/

/*

    println("---------------FILE read start------------------")
    val out = new FileWriter("/home/hc/IdeaProjects/result/routing_200_10run", true)
    out.write("------------------Routing RESULT-----------------\n")
    val xset = List("c","r","rc")
    for(x <- xset){
      for(j <- 1 to 2){
        for(i <- 1 to 10){
          var final_best : Solution = new Solution()
          final_best.cost = Double.PositiveInfinity
          val str = f"pdp_200_union/l$x%s$j%d_2_$i%d.txt"
          out.write(str + "\n")
          val data = new Datas(str)
          data.read1LPDP()
          val startTime = System.currentTimeMillis()    //獲取當前時間
          val lnspdp = new LnsPDP(new InitialSolution(data),1,1)
          for(j <- 1 to 1){
            var sol = lnspdp.tabuLns()
            if(Common.less(sol.cost, final_best.cost))
//            if(Common.less(sol.vehicles.length,final_best.vehicles.length))
              final_best = sol.copySolution()
          }
          out.write(final_best.toString + "\n" )
          out.write("total time: " + (System.currentTimeMillis() - startTime)/10000.0 + "\n")  //單位s
          out.flush()
        }
      }
    }

    out.close()
*/
    /*-------------pdp test end-----------------------*/
    /*--------------3l-pdp test---------------------------------------*/
    println("---------------3l-pdp start-------------------------------")
    val out = new FileWriter("/home/hc/IdeaProjects/result/new/3L-pdp_50_5run_result", true)
    out.write("------------------Routing RESULT-----------------\n")
    val xset = List("CPCD")
//    val xset = List("RAND","CLUS","CPCD")
    for(x <- xset){
        for(j <- 2 to 2){
          for(i<- 1 to 5){
            var final_best : Solution = new Solution()
            final_best.cost = Double.PositiveInfinity
            var totalCost : Double = 0.0
            var totalVolume : Double = 0.0
            val str = f"3L-PDP-instances/050_$x%s_$j%d_$i%d.txt"
//            val str = f"3L-PDP-instances/test.txt"
            println("*****************"+ str+ "******************")
            out.write(str + "\n")
            //      val data = new Datas("3L-PDP-instances/test.txt")
            val data = new Datas(str)
            data.read3LPDP()
            val startTime = System.currentTimeMillis()    //獲取當前時間
            val lnspdp = new LnsPDP(new InitialSolution(data),1,1)

            if(j == 2) lnspdp.setT(60.0) else lnspdp.setT(60.0)
            for(num <- 1 to 5){
              println("ite num: "+ num)
              lnspdp.Maxvolume = 0
              var sol = lnspdp.tabuLns()
              totalCost += sol.cost
              totalVolume += lnspdp.Maxvolume
              out.write("ite:"+num+ " " + sol.toString+ "\n")
              if(Common.less(sol.cost, final_best.cost))
                final_best = sol.copySolution()
            }
            out.write("best cost: "+final_best.toString +"\n")
            out.write("avg cost: " + totalCost/5 + "\n")
            out.write("avg volume:"+ totalVolume/5 + "\n")
            out.write("total time: " + (System.currentTimeMillis() - startTime)/5000.0 + "\n")  //單位s
            println("total time: " + (System.currentTimeMillis() - startTime)/5000.0+ "\n")      //s
            out.flush()
          }
        }
    }
    out.close()


//      val data = new Datas("3L-PDP-instances/050_CLUS_2_4.txt")
//    val data = new Datas("3L-PDP-instances/test.txt")
//      val data = new Datas("pdp_100_union/2.txt")
//    data.read1LPDP()

  }
}
