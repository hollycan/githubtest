package tlpdp.lns.remove

import tlpdp.InitialSolution.InitialSolution
import tlpdp.model.{Solution, SolutionItem, TabuItem}

/**
  * Created by hc on 17-1-12.
  */
class RandomRemove(
    init: InitialSolution
) extends ShawRemove(init){

  /**
    * 當shawRemove p爲1.0時，爲隨機remove
    * @param requestNum
    * @param inSolution
    * @param p
    * @param tabuList
    * @return
    */
  override def removal(requestNum:Int, inSolution : Solution, p : Double, tabuList : Array[TabuItem]): SolutionItem = {
    shawRemove(requestNum,inSolution,1.0,tabuList)
  }
}
