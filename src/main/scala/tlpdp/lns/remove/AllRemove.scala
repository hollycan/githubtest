package tlpdp.lns.remove

import tlpdp.InitialSolution.InitialSolution
import tlpdp.model.Solution

/**
  * Created by hc on 17-2-25.
  */
class AllRemove(
   init : InitialSolution
 ) extends Removal(init){

  /*----------------------移除一條路徑上的所有request---------------------*/
  def allRemove(carOrder : Int,inSolution : Solution,carNum : Int):Solution = {
    getRequest(inSolution)
    var tempVehicle = now.vehicles(carNum-1).copyVehicle()
    now.vehicles(carNum-1) = now.vehicles(carOrder).copyVehicle()
    now.vehicles(carOrder) = tempVehicle.copyVehicle()
    now.vehicles(carOrder).id = carOrder
    now.vehicles(carNum-1).id = carNum-1
    for(r <- now.vehicles(carNum-1).requests){
      delRequest += r
    }
    now = removeDelRequest(delRequest, now)
    now
  }
}
