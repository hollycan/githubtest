package tlpdp.model

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-4-10.
  */
class PackingState(
   var plan : ArrayBuffer[SpaceBox] = new ArrayBuffer[SpaceBox](0),
   var spaceStack : ArrayBuffer[Position] = new ArrayBuffer[Position](0),
   var avail : ArrayBuffer[Box] = new ArrayBuffer[Box](0),
   var planVolume : Double = 0.0,
   var volumeCompelete:Double = 0.0
                  )extends Serializable{



  def copyPackingState() : PackingState = {
    var pl : ArrayBuffer[SpaceBox] = plan.map(c => c.copySpaceBox())    //當元素爲類時一定要copy
    var stack : ArrayBuffer[Position] = spaceStack.map (s => s.copyPosition())
    var av : ArrayBuffer[Box] = avail.map(a => a.copyBox())     //存放當前已服務的request
    val ps = new PackingState(pl,stack,av, planVolume,volumeCompelete)
    ps
  }


}
