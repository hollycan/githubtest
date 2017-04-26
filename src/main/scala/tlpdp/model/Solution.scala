package tlpdp.model

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-1-3.
  */
class Solution (
    val veh : Array[Vehicle] = new Array[Vehicle](0),
    val requestBank : ArrayBuffer[Request] = new ArrayBuffer[Request](0),   //未服務的request
    var cost : Double = 0.0                   //cost = distance + servicetime + missRequest
 )extends Serializable{

  var vehicles : Array[Vehicle] = veh.map(c => c.copyVehicle())       //當元素爲類時一定要copy
  var bank : ArrayBuffer[Request] = requestBank.map (r => r.copyRequest())
  var usefulRequests : ArrayBuffer[Request] = new ArrayBuffer[Request]()     //存放當前已服務的request

  var improvedFlag : Boolean = false      //代表解是否作爲初始解（用於分布式）

  def vehNum : Int = vehicles.length
  def BankNum : Int = requestBank.length

  /**
    * 求解已服務的request
    */
  def useRequest() : Unit = {
    usefulRequests.clear()
    for(v <- vehicles){
      usefulRequests = usefulRequests ++ v.requests
    }
  }

  /**
    * 復制解
    * @return
    */
  def copySolution() : Solution = {
    val solution = new Solution(vehicles, bank, cost)
    solution
  }


 override def toString: String = {
   var i: Int = 0
   var j: Int = 0
   var s : String = "--------------------solution--------------------" + "\n"
   s = s + "Cost: " + cost + "\n"

   s = s + "Distance: " + (cost-1000.0*bank.length) + "\n"
/*   s = s + (
     for(v <- vehicles if v.customers(0) > 0) yield {
        i = i + 1
        "Num." + i + " vehicle :" + v.toString()
     }).mkString("\n")
   for(v <- vehicles if v.customers(0)>0) i= i +1*/
//   s = s + "\n"
   if(!bank.isEmpty){
     s = s + (
       for(r <- bank) yield {
         j = j + 1
         "No serve request" + "Num." + j + r.toString()
       }).mkString("\n")
   }
   for(v <- vehicles if v.customers(0)>0) yield {
     i = i +1
   }
   s = s + "\n"
   s = s + "Carnum: " + i
   s
 }
}
