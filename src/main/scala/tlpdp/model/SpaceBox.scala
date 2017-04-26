package tlpdp.model

/**
  * Created by hc on 16-12-20.
  */
//save box with position for plan
class SpaceBox (
   val space : Position,
   val block : Box
)extends Serializable{

  def copySpaceBox() : SpaceBox = {
    val sb = new SpaceBox(space,block)
    sb
  }

  override def toString(): String = {
    "Box : " + "id " + block.id + "( " + block.l + " ," + block.w + "," + block.h + ")" + space
  }

}
