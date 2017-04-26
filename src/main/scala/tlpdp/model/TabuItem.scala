package tlpdp.model

/**
  * Created by hc on 17-1-11.
  */
/**
  * 將request作爲禁忌表元素
  * @param rid
  * @param t
  */
class TabuItem(
   var rid : Int,
   var t : Int       //禁忌來源 ，0-shawRemove, 1-random, 2-worstRemove
 ) extends Serializable{

 def same(that : TabuItem) : Boolean = {
   if(that== null || that.t != this.t) false
   else{
     this.rid == that.rid
   }
  }
 /**
   * 判斷當前項是否在tabulist中
  */
 def exist(tabuList : Array[TabuItem]):Boolean = {
   tabuList.exists(item => item.same(this))
  }

 def set(that : TabuItem) : Unit = {
  this.rid = that.rid
  this.t = that.t
 }

 override def toString() : String = {
  "tabuitem" + "( type: " + t + " " + "requestId: " + rid + ")"
 }
}
