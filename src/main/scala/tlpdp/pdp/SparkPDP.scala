package tlpdp.pdp

import java.io.FileWriter

import org.apache.spark.{SparkConf, SparkContext}
import tlpdp.InitialSolution.{InitialSolution, greedy}
import tlpdp.common.Common
import tlpdp.model.{Solution, SolutionPool}

/**
  * Created by hc on 17-2-17.
  */
class SparkPDP(
  val fname : String,
  val numPartitions : Int,
  val sc : SparkContext
) extends Serializable{

  val data = new Datas(fname)
  data.read3LPDP()
  //構造初始解
  val initial = new greedy(data)

  //禁忌搜索參數
  val maxTabuIter : Int = 300000

  //最優解
//  var best : Solution = new Solution()
//  best.cost = Double.PositiveInfinity

  var solutionPool : SolutionPool = new SolutionPool(initial)
  //總迭代數
  val segment : Int = 6  //迭代6次收斂
  //當前迭代數
  var nowIte : Int = 0
  //過度數組
  val start : Array[Int] = new Array[Int](numPartitions)

  def reset():Unit = {
    nowIte = 0
    solutionPool = new SolutionPool(initial)
  }

  def paralle(): Solution = {
//    println("---------------Initial Solution-------------------")
    reset()

    val lnspdps : Array[LnsPDP] = (1 to numPartitions).map(i => new LnsPDP(data, numPartitions, i)).toArray
    for(l <- lnspdps){
      l.setMaxIte(maxTabuIter)
    }
//    lnspdps(0).setTemp(60,0.9995)
//    lnspdps(1).setTemp(20,0.95)
    lnspdps(0).setRemoveWeight(0.3,0.7,1.0)
    lnspdps(0).setTemp(30,0.998)
    lnspdps(1).setRemoveWeight(0.4,0.6,1.0)
    lnspdps(1).setTemp(90,0.9)
    lnspdps(2).setRemoveWeight(0.5,0.7,1.0)
    lnspdps(2).setTemp(30,0.998)
    lnspdps(3).setRemoveWeight(0.5,0.7,1.0)
    lnspdps(3).setTemp(90,0.9)

    println("-----------------paralle improve----------------------")
    while(nowIte < segment){
      var startSols : Array[Solution] = new Array[Solution](numPartitions)
      if(nowIte==0){
        startSols = initial.greedyMultInitSolution(numPartitions).toArray
//        val oneSolutionRdd = sc.parallelize(initial.greedyMultInitSolution(numPartitions),numPartitions)
      }
      else{
        val startSol = solutionPool.startSolution()
        startSols = start.map(_ => startSol)

      }
//      val oneSolutionRdd = sc.parallelize(initial.greedyMultInitSolution(numPartitions),numPartitions)
      val oneSolutionRdd = sc.parallelize(startSols,numPartitions)
      var lnsRdd = sc.parallelize(lnspdps, numPartitions)
      val mergeRdd = oneSolutionRdd.zip(lnsRdd).map(m => {
        m._2.setInitial(m._1)
        m._2.ts()
        m._2.checkSolution(m._2.best)
        if(!m._2.feasible3DLoading(m._2.best.vehicles)) println("loading error")
        m._2.best
      }).collect()

      //將當此迭代每塊add中最好解放入solutionPool中
      mergeRdd.foreach(s =>{
//        if(Common.more(best.cost , s.cost)) best = s.copySolution()
          solutionPool.add(s)
      })
      println("ite num: "+ nowIte + "\n" + solutionPool.best)
      nowIte += 1
    }



    println(solutionPool.best)
//    solutionPool.best.toString
    solutionPool.best
  }

}

object SparkPDP{

  def main(args:Array[String]):Unit = {
    val conf = new SparkConf().setMaster("local").setAppName("version1_pdp")
    val sc = new SparkContext(conf)

    /*    val writeOut = new FileWriter("/home/hc/IdeaProjects/result/test1", true)
    val fname = "3L-PDP-instances/050_RAND_2_1.txt"
    writeOut.write("-----------------file name: "+fname + "------------------")
    println("data:" + fname)
    val numPartitions : Int = 4                 //分塊數量
    val startTime = System.currentTimeMillis()    //獲取當前時間
    val pdp = new SparkPDP(fname, numPartitions, sc)
    writeOut.write(pdp.paralle())
    println("total time: " + (System.currentTimeMillis() - startTime)/60000.0)
    writeOut.flush()
    writeOut.close()

  }*/

    /*--------------3l-pdp test---------------------------------------*/
    println("---------------3l-pdp start-------------------------------")
    val out = new FileWriter("/home/hc/IdeaProjects/result/new/3L-pdp_spark_5run_075reslut", true)
    out.write("------------------Routing RESULT-----------------\n")
    val xset = List("RAND","CLUS","CPCD")
//        val xset = List("CPCD")
    for (x <- xset) {
      for (j <- 2 to 3) {
        for (i <- 1 to 2) {
          var final_best: Solution = new Solution()
          final_best.cost = Double.PositiveInfinity
          var totalCost: Double = 0.0
          val str = f"3L-PDP-instances/075_$x%s_$j%d_$i%d.txt"
          println("*****************" + str + "******************")
          out.write(str + "\n")
          val numPartitions : Int = 4
          val startTime = System.currentTimeMillis()
          //獲取當前時間
          val sparkpdp = new SparkPDP(str, numPartitions, sc)
//          lnspdp.Maxvolume = 0
//          if (j == 2) lnspdp.setT(2.0) else lnspdp.setT(5.0)
          for (num <- 1 to 5) {
            println("ite num: " + num)
            var sol = sparkpdp.paralle()
            totalCost += sol.cost
            out.write("ite:" + num + " " + sol.toString + "\n")
            if (Common.less(sol.cost, final_best.cost))
              final_best = sol.copySolution()
          }
          out.write("best cost: " + final_best.toString + "\n")
          out.write("avg cost: " + totalCost / 5 + "\n")
          out.write("total time: " + (System.currentTimeMillis() - startTime) / 5000.0 + "\n") //單位s
          println("total time: " + (System.currentTimeMillis() - startTime) / 5000.0 + "\n") //s
          out.flush()
        }
      }
    }
    out.close()
  }
}
