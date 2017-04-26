package tlpdp.model

/**
  * Created by hc on 16-12-15.
  */

//place position of item
class Position(
    val x : Double = 0.0,
    var y : Double = 0.0,
    val z : Double = 0.0
) extends Serializable{
  //剩餘空間長度
  var lx : Double = Double.MaxValue
  var ly : Double = Double.MaxValue
  var lz : Double = Double.MaxValue
  var fragility : Boolean = false         //當z不爲0時，判斷該點下邊的物品的易碎性

  def area : Double = lx * ly

  def copyPosition():Position = {
    val po = new Position(x,y,z)
    po.lx = lx
    po.ly = ly
    po.lz = lz
    po.fragility = fragility
    po
  }
  override def toString(): String ={
    "Position:" + ":(" + x + "," + y + "," + z + ")" + "remainSpace:" + "lx:" + lx + "ly:" + ly + "lz:" + lz
  }
}
