package tlpdp.lns.remove

import tlpdp.InitialSolution.InitialSolution
import tlpdp.common.Common
import tlpdp.model._

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

/**
  * Created by hc on 17-1-10.
  */
class WorstRemove (
 /*    requestNum : Int,
     inSolution : Solution,*/
     init : InitialSolution
 //    p : Double
 )extends Removal(init){

  override def removal(requestNum:Int, inSolution : Solution, p : Double, tabuList : Array[TabuItem]): SolutionItem = {
    worstRemove(requestNum,inSolution,p,tabuList)
  }
  //---------------------------------------------two----------------------------------------------------------

  def worstRemove(requestNum:Int, inSolution : Solution, p : Double, tabuList : Array[TabuItem]):SolutionItem = {
    val needTabu : ArrayBuffer[TabuItem] = new ArrayBuffer[TabuItem]()                //最終需要tabu的項,根據requestNum數量禁忌
    var tmpTabue = new TabuItem(0,0)
    getRequest(inSolution)
    while(delRequest.length < requestNum){
      var delcost : Array[Relate] = (
        for(i <- 0 until useRequests.length) yield {
          val carId = findCar(useRequests(i), now.vehicles)
          val pid = findId(useRequests(i).pick.id, now.vehicles(carId).customers)
          val did = findId(useRequests(i).delivery.id, now.vehicles(carId).customers)
          val tmp = delCost(pid, did, now.vehicles(carId).customers)
          val cost = if(Common.eq(tmp,0.0)) 0.0 else -tmp
          new Relate(i, 0 , cost)
        }).toArray
      delcost = delcost.sorted
      var y = Common.rand.nextDouble()
      while(Common.eq(y,0.0))
        y = Common.rand.nextDouble()
      var del : Int = delcost(((1-math.pow(y,p))* (delcost.length-1)).toInt).i
      val delcopy : Int= del                 //保存邊界情況
      var flag : Boolean = true
      tmpTabue = setTabu(useRequests(del).id,2)
      while(Common.isTabu(tmpTabue,tabuList) && flag){           //如果所有可行request都被禁忌，則取最先獲取的request
        del = (del +1) % useRequests.length                //如果request被禁忌，則取數組後一個request
        if(del == delcopy) flag = false
        tmpTabue = setTabu(useRequests(del).id, 2)
      }
      if(flag) needTabu += tmpTabue
      delRequest += useRequests(del)
      useRequests.remove(del)
    }
    now = removeDelRequest(delRequest, now)
    new SolutionItem(now, needTabu)
  }
  //---------------------------------------------two----------------------------------------------------------
  /**
    * 搜索request屬於哪輛車
    */

  def findCar(req : Request, veh : Array[Vehicle]) : Int = {
    var id : Int =0
    var flag : Boolean  = false

      for( i <- 0 until veh.length){
        for(j <- 0 until veh(i).requests.length){
          if(!flag && req.equals(veh(i).requests(j))){
            id = i
            flag = true

          }
        }
      }

    id
  }
}
