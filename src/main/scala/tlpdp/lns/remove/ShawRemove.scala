package tlpdp.lns.remove

import tlpdp.InitialSolution.InitialSolution
import tlpdp.common.Common
import tlpdp.model.{Solution, SolutionItem, TabuItem}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-1-10.
  */
class ShawRemove (
/*   requestNum : Int,
   inSolution : Solution,*/
   init : InitialSolution
//   p : Double
)extends Removal(init){

  override def removal(requestNum:Int, inSolution : Solution, p : Double, tabuList : Array[TabuItem]): SolutionItem = {
    shawRemove(requestNum,inSolution,p,tabuList)
  }
  //-----------------------------------------------one------------------------------------------------
  /**
    * 第一種remove優化，shawRemoval
    * @return
    */
  def shawRemove(requestNum:Int, inSolution : Solution, p : Double, tabuList : Array[TabuItem]) : SolutionItem = {
    getRequest(inSolution)
    val needTabu : ArrayBuffer[TabuItem] = new ArrayBuffer[TabuItem]()                //最終需要tabu的項,根據requestNum數量禁忌
    var tmpTabue = new TabuItem(0,0)
    var r = Common.rand.nextInt(useRequests.length)
    tmpTabue = setTabu(useRequests(r).id,0)
    while(Common.isTabu(tmpTabue,tabuList)){                    //判斷是否被禁忌
      r=Common.rand.nextInt(useRequests.length)
      tmpTabue = setTabu(useRequests(r).id,0)
    }
    needTabu += tmpTabue
    delRequest += useRequests(r)
    useRequests.remove(r)                                //刪除被remove的request
    while(delRequest.length < requestNum){
      r = Common.rand.nextInt(delRequest.length)
      var relatePairs : Array[Relate] = (
        for (i <- 0 until useRequests.length) yield {
          new Relate(r , i , init.rel(delRequest(r).id-1, useRequests(i).id-1))                    //將delRequest裏邊獲取的request和useRequest比較相似性
        }).toArray
      relatePairs = relatePairs.sorted
      var y = Common.rand.nextDouble()                  //生成0,1之間隨機數
      while(Common.eq(y,0.0))
        y = Common.rand.nextDouble()
      var del : Int = relatePairs((math.pow(y,p) * (relatePairs.length-1)).toInt).j
      val delcopy : Int= del                 //保存邊界情況
      var flag : Boolean = true
      tmpTabue = setTabu(useRequests(del).id,0)
      while(Common.isTabu(tmpTabue,tabuList) && flag){           //如果所有可行request都被禁忌，則取最先獲取的request
        del = (del +1) % useRequests.length                //如果request被禁忌，則取數組後一個request
        if(del == delcopy) flag = false
        tmpTabue = setTabu(useRequests(del).id,0)
      }
      if(flag) needTabu += tmpTabue
      delRequest += useRequests(del)                    // 先獲取relatePairs數組中元素獲取元素中j值（即在useRequests中的位置）
      useRequests.remove(del)
    }
    now = removeDelRequest(delRequest, now)
    new SolutionItem(now, needTabu)
  }

  //---------------------------------------------one----------------------------------------------------------

}
