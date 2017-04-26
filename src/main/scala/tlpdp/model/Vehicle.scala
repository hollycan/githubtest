package tlpdp.model

import tlpdp.common.Common

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 16-12-15.
  */
class Vehicle(
  var id : Int,
  val capacity : Double = Double.MaxValue,               //負載
  val height : Double = Double.MaxValue,                 //車版的長寬高
  val width : Double = Double.MaxValue,
  val length : Double = Double.MaxValue
//  val maxDistance : Double = Double.PositiveInfinity
) extends Serializable{

//  var nowCapacity :  Double = 0.0        //當前栽種
  var cost : Double = 0.0                //當前路徑代價 (代價包括distance + serviceTime）
  var customers : ArrayBuffer[Int] = new ArrayBuffer[Int]()            //乘客序列,不需要depot
  var nowCapacity : ArrayBuffer[Double] = new ArrayBuffer[Double]()         //當前位置車輛載重量
  var requests : ArrayBuffer[Request] = new ArrayBuffer[Request]()           //存儲的需求序列
  var distance : Double = 0.0             //距離
  var serviceTime : Double = 0.0

  def reset():Unit = {
    cost = 0.0
    distance = 0.0
    customers.clear()
    nowCapacity.clear()
    requests.clear()
    init()
  }
  def init() : Unit = {
    customers += 0
    nowCapacity += 0
  }
  /**
    * 用於解的復制
    * @return Vehicle
    */
  def copyVehicle() : Vehicle = {
    copyVehicle(customers, nowCapacity, requests)
  }

  def copyVehicle(cus : ArrayBuffer[Int] , cap : ArrayBuffer[Double] , req : ArrayBuffer[Request]) : Vehicle = {
    val vehicle = new Vehicle(id,capacity,height,width,length)
    vehicle.nowCapacity = cap.clone()
    vehicle.cost = cost
    vehicle.customers = cus.clone()
    vehicle.requests = req.clone()
    vehicle.distance = distance
    vehicle.serviceTime = serviceTime
    vehicle
  }

  override def toString: String = {
    var st = "(vehicle : " + "carId: " + id + " cost: " + cost + " distance: "+ distance + " serviceTime: " + serviceTime+
      ")"
    st = st + (
      for(i <- 1 to customers(0)) yield {
        customers(i).toString()
      }).mkString(",")
    st = st + "\n" + (
      for(j <- 0 until requests.length) yield {
        requests(j).toString()
      }).mkString("\n ")
    st
  }
}
