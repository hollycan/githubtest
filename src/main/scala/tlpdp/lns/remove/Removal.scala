package tlpdp.lns.remove

import tlpdp.InitialSolution.InitialSolution
import tlpdp.model._
import tlpdp.common.Common

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

/**
  * Created by hc on 17-1-7.
  */
class Removal(
  /* val requestNum : Int,
   val inSolution : Solution , */       //傳入的解
   val init : InitialSolution             //過度類，用於獲取cost以及relatedness數組
//   val p : Double = 1.0               //當p爲1時爲random removal
            ) extends Serializable{

   //存儲的是useRequest裏邊的id (當j爲0,用於存儲worst delcost費用）
   class Relate(val i : Int, val j : Int , val relateness : Double) extends Ordered[Relate] {
     def compare(that: Relate): Int = {
       //順序
       val co = this.relateness - that.relateness
       if (co.abs <= 1e-6) 0
       else if (co > 0) 1
       else -1
     }
   }
   var delRequest : ArrayBuffer[Request] =  new ArrayBuffer[Request]()       //被選擇需要刪除的request

   var useRequests : ArrayBuffer[Request] = new ArrayBuffer[Request]()        //已經被服務的requests

   var now : Solution = new Solution()

   val needTabu : ArrayBuffer[TabuItem] = new ArrayBuffer[TabuItem]()


  /**
    * 獲取當前解的request
    */
   def getRequest(sol : Solution):Unit ={
     sol.useRequest()
     useRequests  = sol.usefulRequests.clone()        //已經被服務的requests
     now = sol.copySolution()
     delRequest.clear()
   }

   def removal(requestNum:Int, inSolution : Solution, p : Double, tabuList : Array[TabuItem]) : SolutionItem ={
     new SolutionItem(now,needTabu)
   }

   def setTabu (rid : Int, t : Int) : TabuItem = {
    new TabuItem(rid, t)
  }
  /**
    * 當前解刪除delRequest裏邊所有的request
    * @param dr
    * @param sol
    */
   def removeDelRequest(dr : ArrayBuffer[Request] , sol : Solution) : Solution= {
     var temp = sol.copySolution()
     for(r <- dr)
      temp = removeOneRequest(r,temp)
//     sol.cost = init.calCost(sol.vehicles)
//     sol.cost = init.calDistance(sol.vehicles)
//    temp.requestBank = temp.bank.map (r => r.copyRequest())
     for(v <- temp.vehicles){
       if(v.customers(0)==0) v.distance=0
     }
     temp.cost = init.calDistance(temp.vehicles)
     temp.cost = init.calCostAddMissRequest(init.M, temp.cost, temp.bank.length)
     temp
   }

   def removeOneRequest(r : Request , sol: Solution) : Solution = {
     val temp = sol.copySolution()
     temp.bank += r
     var flag : Boolean = false

       for(v <- temp.vehicles){
         for(i <- 0 until v.requests.length){
           if(!flag && r.equals(v.requests(i))){
              v.requests.remove(i)
              val pid = findId(r.pick.id, v.customers)
              val did = findId(r.delivery.id, v.customers)
              v.distance = v.distance + delCustomer(pid , did, v.customers)           //刪除一條路徑上乘客
              delCapacity(v.nowCapacity, pid, did, r.boxesWeight)                    //修改capacity數組
              flag = true

           }
         }
       }
     temp
   }

  /**
    * search pick and delivery 在customers位置
    */
  def findId(i : Int, cus : ArrayBuffer[Int]) : Int = {
    var id : Int =0
    var flag:Boolean = false
      for(s <- 1 until cus.length if !flag){
        if(i == cus(s)){
          id = s
          flag = true
        }
      }
    id
  }
  /**
    *  刪除一條路徑上乘客，返回刪除的cost (修改vehicle的customer及capicity）
    * @param i
    * @param j
    * @param cus
    * @return
    */
   def delCustomer(i : Int , j : Int , cus : ArrayBuffer[Int]) : Double = {
     var c = delCost(i,j,cus)
     cus.remove(j)                //先刪除後邊的元素
     cus.remove(i)
     cus(0) = cus(0)-2
     c
   }

  /**
    * 刪除request 增加的費用
    * @param i
    * @param j
    * @param cus
    * @return
    */
  def delCost(i : Int , j : Int , cus : ArrayBuffer[Int]) :  Double = {
    var c : Double = 0.0
    val pl = if (i==1) 0 else cus(i-1)
    val pr = if(i == j-1) cus(j) else cus(i+1)
    val dl = if(i == j-1) cus(i) else cus(j-1)
    val dr = if (j == cus(0)) 0 else cus(j+1)

    if(i == j-1){
      c = c - init.dis(pl, cus(i)) - init.dis(pr,dl) - init.dis(cus(j),dr) + init.dis(pl, dr)
    }else{
      c = c - init.dis(pl, cus(i)) - init.dis(cus(i), pr) + init.dis(pl, pr) - init.dis(dl, cus(j)) - init.dis(cus(j),dr) + init.dis(dl,dr)
    }
    c
  }
  /**
    * 修改capacity數組
    */
   def delCapacity(cap : ArrayBuffer[Double], i : Int , j : Int , weight:Double) :Unit ={
     for(ite <- i+1 to j-1) cap(ite) = cap(ite) - weight
     cap.remove(j)
     cap.remove(i)
   }

}
