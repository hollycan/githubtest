package tlpdp.pdp

import java.io.FileWriter

import tlpdp.model.Request

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by hc on 17-1-13.
  */
/**
  * 用來處理pdp_100數據
  * @param fname
  */
class DataHandle(val fname : String, out:FileWriter) {
  var requests : ArrayBuffer[Request] = new ArrayBuffer[Request]()

  def changeData () : Unit = {
    println("--------------start ------------------")
    var oneLineData : Array[Int] = new Array[Int](0)
    val lineIte: Iterator[String] = Source.fromFile(fname).getLines()
    oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toInt)
    //第一行數據保留
    out.write(oneLineData(0) + " " + oneLineData(1) + " " + oneLineData(2) + "\n")

    //第二行數據保留
    out.write(lineIte.next() + "\n")
    var lineData : ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
    lineIte.foreach { lineData += _.split("\\s+").filter(!_.isEmpty()).map(_.toInt) }
    val newData : ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
    var num : Int = 0
    for(i <- 0 until lineData.length ){
      if(lineData(i)(8) != 0){
        num += 1
        val tmp : Int= lineData(i)(8)
        var arr : Array[Int] = lineData(i)
        var arra : ArrayBuffer[Int] = ArrayBuffer[Int]()
        arra ++= arr
        arra += (lineData(tmp-1)(1) , lineData(tmp-1)(2) , lineData(tmp-1)(4) , lineData(tmp-1)(5) , lineData(tmp-1)(6) )
        arra(0) = num
        for(elem <- arra)
          out.write(elem + "\t")
        out.write("\n")
      }
    }
    println("------------end--------------")
    out.flush()

  }
}

object DataHandle{

  def main(args : Array[String]):Unit= {
    for(i <- 10 to 10){
      val str = f"pdp_200/LRC1_2_$i%d.txt"
      val out = new FileWriter(f"pdp_100_union/" + f"lrc1_2_$i%d.txt")
      val D : DataHandle = new DataHandle(str,out)
      D.changeData()
      out.close()
    }

  }

}