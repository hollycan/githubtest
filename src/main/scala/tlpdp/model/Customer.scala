package tlpdp.model

/**
  * Created by hc on 17-1-3.
  */
class Customer (
   val id : Int ,                 //乘客id, depot o
   val x : Double = 0.0,
   val y : Double = 0.0,            //坐標
   val ServiceTime : Double = 0.0, //service time
   val requestId : Int ,             //附屬的request id
   val character : Int,              // 0 as pick ; 1 as delivery; 2 as depot
   //時間窗
   val earlyTime : Double = 0.0 ,              //earliest time
   val latestTime : Double = 0.0               //latest time
 ) extends Serializable{
  /**
    * 兩個customer距離
    */
  def dis(that : Customer) : Double = {
    Math.sqrt( Math.pow((this.x-that.x),2) + Math.pow((this.y-that.y),2) )
  }

  override def toString: String = {
    "Customer:"+ id +": (" + x + "," + y +") "
  }
}
