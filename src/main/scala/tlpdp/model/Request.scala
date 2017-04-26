package tlpdp.model

/**
  * Created by hc on 16-12-15.
  */
class Request(
               val id : Int, //number of request
   /*          val Px : Double = 0.0, //position x,y of pickup
               val Py : Double = 0.0,
               val PServiceTime : Double = 0.0, //service time of pickup
               val Dx : Double = 0.0, //position x,y of deliver
               val Dy : Double = 0.0,
               val DServiceTime : Double = 0.0, //service time of deliver
               */
               val pick : Customer,
               val delivery : Customer,
               val boxesWeight : Double = 0.0, //box 總重量
               val num : Int = 0, //number of boxes
               val boxes : Array[Box] = Array() //demand boxes
)extends Serializable{

  def totalVolume():Double = {
    var volume : Double =0.0
    for(v <- boxes)
      volume += v.boxVolume
    volume
  }

  def totalDistance():Double = {
    var distance:Double = 0.0
    distance = Math.sqrt(Math.pow(pick.x - 40.0,2) + Math.pow(pick.y - 50.0, 2))
    distance += Math.sqrt(Math.pow(delivery.x - 40.0,2) + Math.pow(delivery.y - 50.0, 2))
    distance
  }

  def copyRequest() : Request = {
   val request = new Request(id,pick,delivery,boxesWeight,num,boxes)
   request
  }

  override def toString: String = {
    "Request " + id + ": pick Customer [ " + pick.toString() + " ]delivery Customer[ " + delivery.toString() + "]"
  }
}
