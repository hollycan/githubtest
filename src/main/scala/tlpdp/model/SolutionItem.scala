package tlpdp.model

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-1-12.
  */
/**
  * 用於remove之後返回需解及需要禁忌的項
  * @param sol
  * @param items
  */
case class SolutionItem(sol : Solution , items : ArrayBuffer[TabuItem]) extends Serializable


