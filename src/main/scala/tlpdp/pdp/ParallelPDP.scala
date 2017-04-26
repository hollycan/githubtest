package tlpdp.pdp

import tlpdp.InitialSolution.greedy
import tlpdp.model.{Solution, SolutionPool}

/**
  * Created by hc on 17-3-7.
  */
class ParallelPDP (
  val fname : String,
  val numPartitions : Int
  )extends Serializable{

  val data = new Datas(fname)
  data.read3LPDP()
  //構造初始解
  val initial = new greedy(data)

  //禁忌搜索參數
  val maxTabuIter : Int = 1000

  val solutionPool : SolutionPool = new SolutionPool(initial)
  //總迭代數
  val segment : Int = 10   //信息匯總次數
  //當前迭代數
  var nowIte : Int = 0
  //過度數組
  val start : Array[Int] = new Array[Int](numPartitions)

  def paralle(): Unit = {
    println("---------------start-------------------")

    val lnspdps : Array[LnsPDP] = (1 to numPartitions).map(i => new LnsPDP(data, numPartitions, i)).toArray
    for(l <- lnspdps){
      l.setMaxIte(maxTabuIter)
    }
    //    lnspdps(0).setTemp(60,0.9995)
    //    lnspdps(1).setTemp(20,0.95)
    //    lnspdps(0).setTabuLen(1)

    println("-----------------paralle improve----------------------")
    while(nowIte < segment){
      var startSol : Solution = new Solution()
      if(nowIte==0){
        startSol = initial.greedyInitSolution()
        //        val oneSolutionRdd = sc.parallelize(initial.greedyMultInitSolution(numPartitions),numPartitions)
      }
      else{
        val startSol = solutionPool.startSolution()

      }
      //      val oneSolutionRdd = sc.parallelize(initial.greedyMultInitSolution(numPartitions),numPartitions)
      val solutions = for(i<- 0 until numPartitions)yield{
        val pdp = lnspdps(i)
        pdp.setInitial(startSol)
        pdp.ts()
        pdp.best
      }

      (0 until numPartitions).foreach(i => {
        val s = solutions(i)
        solutionPool.add(s)
      })
      //將當此迭代每塊add中最好解放入solutionPool中

//      println("ite num: "+ nowIte + "\n" + solutionPool.best)
      nowIte += 1
    }



    println(solutionPool.best)
    solutionPool.best.toString
  }
}
