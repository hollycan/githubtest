package tlpdp.common

import tlpdp.model.TabuItem

import scala.util.Random

/**
  * Created by hc on 17-1-3.
  * common function
  */
object Common {

  val rand : Random = new Random()
  //Double 小于
  def less(a: Double, b : Double ) : Boolean =
    a<b && (a-b).abs > 1e-6
  //Double 大于
  def more(a:Double, b: Double) : Boolean =
    a>b && (a-b).abs > 1e-6
  //Double 小于等于
  def le(a: Double, b: Double) : Boolean =
    (a-b).abs < 1e-6 || a<b
  //Double 大于等于
  def ge(a: Double, b: Double) :Boolean =
    (a-b).abs < 1e-6 || a>b
  def eq(a: Double, b: Double) : Boolean =
    (a-b).abs < 1e-6
  def abs(x:Double) :Double=
    if (less(x,0)) -x else x

  /**
    * 在禁忌表中爲true，否則爲false
    * @param tabuItem
    * @param tabuList
    * @return
    */
  def isTabu (tabuItem: TabuItem, tabuList : Array[TabuItem]) : Boolean = {
    tabuList.exists(tabu => tabuItem.same(tabu))
  }
}
