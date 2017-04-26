package tlpdp.pdp

import tlpdp.model.{Box, Customer, Request, Vehicle}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by hc on 17-1-3.
  */
class Datas (val fname : String) extends Serializable{

  var vehicleMaxNum : Int = 0
  var requests : Array[Request] = new Array[Request](0)
  var customers : Array[Customer] = new Array[Customer](0)         // 0爲depot
  var cus : ArrayBuffer[Customer] = new ArrayBuffer[Customer]()    //隨時存儲乘客
  var vehicle : Vehicle = new Vehicle(0)

  def read3LPDP() : Unit = {
    println("--------------start read file------------------")
    var oneLineData : Array[Double] = new Array[Double](0)
    val lineIte: Iterator[String] = Source.fromFile(fname).getLines()
    //general data
    oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)
    vehicleMaxNum = oneLineData(0).toInt
    val numReq : Int = oneLineData(1).toInt             //number of request

    //vehicle
    vehicle = new Vehicle(0, oneLineData(2), oneLineData(5), oneLineData(4), oneLineData(3))
    vehicle.init()

    //depot
    oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)
    val depotX = oneLineData(1)
    val depotY = oneLineData(2)
    cus += new Customer(0, depotX, depotY, 0.0, 0,2)

    //requests
    var boxid:Int = 0
    var customersid : Int =0
    val tmp = new Array[Int](numReq)            //用於request map轉換
    requests =
      tmp.map ( r => {
      oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)
      //box
      val boxes : Array[Box]=
        (for(i <- 0 until oneLineData(8).toInt) yield {
          boxid = boxid +1
          var frag = if(oneLineData(8+i*4+4).toInt==0) false else true
          new Box (oneLineData(8+i*4+3), oneLineData(8+i*4+2),oneLineData(8+i*4+1),frag, boxid)
        }).toArray
      //pick
      customersid = customersid + 1
      val pick : Customer = new Customer(customersid, oneLineData(1),oneLineData(2),oneLineData(3),oneLineData(0).toInt, 0, 0.0, 0.0)
      cus += pick
      //delivery
      customersid = customersid + 1
      val delivery : Customer = new Customer(customersid, oneLineData(4),oneLineData(5),oneLineData(6),oneLineData(0).toInt, 1, 0.0, 0.0)
      cus += delivery
      new Request(oneLineData(0).toInt, pick, delivery, oneLineData(7), oneLineData(8).toInt, boxes)
    })

    customers = (
      for(i <- 0 until cus.length) yield cus(i)
    ).toArray
    println("------------input end-----------------")

  }

  def read1LPDP() : Unit = {
    println("------------------start read file--------------------")
    var oneLineData : Array[Double] = new Array[Double](0)
    var lineIte: Iterator[String] = Source.fromFile(fname).getLines()
    //general data
    oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)
    vehicleMaxNum = oneLineData(0).toInt
    val numReq : Int = lineIte.length -1            //number of request length後到文件尾部
    lineIte = Source.fromFile(fname).getLines()
    lineIte.next()
    //vehicle
    vehicle = new Vehicle(0, oneLineData(1), 0.0, 0.0, 0.0)
    vehicle.init()

    //depot
    oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)
    val depotX = oneLineData(1)
    val depotY = oneLineData(2)
    cus += new Customer(0, depotX, depotY, 0.0, 0,2,0.0, oneLineData(5))

    //requests
    var boxid:Int = 0
    var customersid : Int =0
    val tmp = new Array[Int](numReq)            //用於request map轉換
    requests =
      tmp.map ( r => {
        oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)

        //box
        val boxes : Array[Box]= Array[Box]()
        //pick
        customersid = customersid + 1
        val pick : Customer = new Customer(customersid, oneLineData(1),oneLineData(2),oneLineData(6),oneLineData(0).toInt, 0, oneLineData(4), oneLineData(5))
        cus += pick
        //delivery
        customersid = customersid + 1
        val delivery : Customer = new Customer(customersid, oneLineData(9),oneLineData(10),oneLineData(13),oneLineData(0).toInt, 1, oneLineData(11), oneLineData(12))
        cus += delivery
        new Request(oneLineData(0).toInt, pick, delivery, oneLineData(3), 0, boxes)
      })

    customers = (
      for(i <- 0 until cus.length) yield cus(i)
      ).toArray
    println("------------input end-----------------")

  }
}
object Datas{
  def main(args : Array[String]):Unit= {
//    val f = new Datas("3L-PDP-instances/050_CLUS_2_1.txt")
    val f = new Datas("pdp_100_union/1.txt")
    f.read1LPDP()
  }
}
