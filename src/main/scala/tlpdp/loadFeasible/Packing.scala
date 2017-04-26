package tlpdp.loadFeasible

import tlpdp.common.Common
import tlpdp.model.{Box, Position, SpaceBox}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hc on 17-3-23.
  */
class Packing (
  val container : Position,      //(0,0,0)+存放空間長寬高
  val boxList : ArrayBuffer[Box]  //box list
  )extends Serializable{

  /**
    * init parameter
    */
  val carL = container.lx            //車廂的長寬高
  val carW = container.ly
  val carH = container.lz
  var plan : ArrayBuffer[SpaceBox] = new ArrayBuffer[SpaceBox]()
  //the plan of box place
  var planVolume : Double = 0
  //the total volume of the placed box
  var spaceStack : ArrayBuffer[Position] = new ArrayBuffer[Position]()
  //init spacestack
  var matrix3D = Array.ofDim[Boolean](carL.toInt+1,carW.toInt+1,carH.toInt+1)
  //三維坐標
  var avail : ArrayBuffer[Box] = new ArrayBuffer[Box]()
  //copy boxList/available box
  var blockList : ArrayBuffer[Box] = new ArrayBuffer[Box]()
  var blockFromPs : Box = new Box()
  var spaceFromStack : Position = new Position()

  val Percent : Double = 0.75

  def reset():Unit = {
    for(i<- 0 until carL.toInt)
      for(j<- 0 until carW.toInt)
        for(k <- 0 until carH.toInt)
          matrix3D(i)(j)(k) = false
    spaceStack.clear()
    spaceStack += container
    plan.clear()
    planVolume = 0.0
  }

  def packing():Boolean = {
    reset()

    //sort by box volume
    avail = boxList.sortWith(_.area > _.area)
//    avail = boxList.sortWith(_.boxVolume < _.boxVolume)
    avail = avail.sortWith(_.fragility < _.fragility)
    while(!isSpaceEmpty() && avail.nonEmpty) {
      spaceStack = spaceStack.sortWith(_.z < _.z)
      spaceStack = spaceStack.sortWith(_.x < _.x)
      spaceStack = spaceStack.sortWith(_.y < _.y)

      spaceFromStack = spaceStack(0) //get top of stack
      for (b <- avail) b.resetDirection()
      blockList.clear()
      genBlockList(spaceFromStack, plan) //update blocklist
      spaceStack.remove(0) //delete the first one of stack
      if (!isBlockListEmpty()) {
        blockFromPs = blockList(0)
        availRemove(blockFromPs)
        plan += new SpaceBox(spaceFromStack, blockFromPs)
        planVolume += blockFromPs.boxVolume
        genResidulSpace(spaceFromStack, blockFromPs) //更新spacestack空間
      }
    }
    if (avail.isEmpty && checkPlan())
      true
    else
      packing(1)
    /*true else false *///測試test不可能所有箱子都能裝載(需要把avail。isEmpty注釋）
  }

  def packing(index : Int):Boolean = {
    reset()

    //sort by box volume
//    avail = boxList.sortWith(_.area > _.area)
    avail = boxList.sortWith(_.boxVolume < _.boxVolume)
    avail = avail.sortWith(_.fragility < _.fragility)
    while(!isSpaceEmpty() && avail.nonEmpty) {
      spaceStack = spaceStack.sortWith(_.z < _.z)
      spaceStack = spaceStack.sortWith(_.x < _.x)
      spaceStack = spaceStack.sortWith(_.y < _.y)

      spaceFromStack = spaceStack(0) //get top of stack
      for (b <- avail) b.resetDirection()
      blockList.clear()
      genBlockList(spaceFromStack, plan) //update blocklist
      spaceStack.remove(0) //delete the first one of stack
      if (!isBlockListEmpty()) {
        blockFromPs = blockList(0)
        availRemove(blockFromPs)
        plan += new SpaceBox(spaceFromStack, blockFromPs)
        planVolume += blockFromPs.boxVolume
        genResidulSpace(spaceFromStack, blockFromPs) //更新spacestack空間
      }
    }
    if (avail.isEmpty && checkPlan()) true else false //測試test不可能所有箱子都能裝載(需要把avail。isEmpty注釋）
  }

    /**
      * 判斷可選放置空間是否爲空
      */
    def isSpaceEmpty(): Boolean = if (spaceStack.length == 0) true else false

    /**
      * 判斷生成的可行裝載塊是否爲空
      */
    def isBlockListEmpty(): Boolean = if (blockList.length == 0) true else false

    //remove current box from avai
    def availRemove(removeBlock: Box): Unit = {
      var flag: Boolean = false
      for (b <- 0 until avail.length if !flag) {
        if (avail(b).id == removeBlock.id) {
          avail.remove(b)
          flag = true
        }
      }
    }

    /**
      * genResidulSpace(space,box) output space
      */
    def genResidulSpace(space: Position, block: Box): Unit = {
      for (i <- space.x.toInt  to space.x.toInt + block.l.toInt)
        for (j <- space.y.toInt  to space.y.toInt + block.w.toInt)
          for (k <- space.z.toInt  to space.z.toInt + block.h.toInt) {
            matrix3D(i)(j)(k) = true
          }
      var newPosition1: Position = new Position(space.x + block.l, space.y, space.z)
      var newPosition2: Position = new Position(space.x, space.y + block.w, space.z)
      var newPosition3: Position = new Position(space.x, space.y, space.z + block.h)
      if (block.fragility) newPosition3.fragility = true
      spaceStack.insert(0, newPosition1, newPosition2, newPosition3)
      removeRepeat() //去除重復點
    }

    /**
      * 去除重復可行點，排除position相同，可行放置域不同的點
      */
    def removeRepeat(): Unit = {
      val copy: ArrayBuffer[Position] = copySpack(spaceStack)
      for (i <- 0 until copy.length) {
        for (j <- i + 1 until copy.length) {
          if (Common.eq(copy(i).x, copy(j).x) && Common.eq(copy(i).y, copy(j).y) && Common.eq(copy(i).z, copy(j).z) &&
            (!Common.eq(copy(i).lx, copy(j).lx) || !Common.eq(copy(i).ly, copy(j).ly) || !Common.eq(copy(i).lz, copy(j).lz))) {
            copy(j) = copy(i)
          }
        }
      }
      spaceStack.clear()
      spaceStack ++= copy.distinct
    }

    /**
      * 復制spaceStack
      */
    def copySpack(sp: ArrayBuffer[Position]): ArrayBuffer[Position] = {
      var copy: ArrayBuffer[Position] = new ArrayBuffer[Position]()
      copy ++= sp
    }

    /**
      * genBlockList (space,avail)   output blockList      //genenrate available boxes in current space
      */
    def genBlockList(sp: Position, placedBox: ArrayBuffer[SpaceBox]): Unit = {
      var flag = false
      for (avBox <- avail if !flag) {
        if (isFeasiblePack(avBox, sp, placedBox))
          {
            blockList += avBox
            flag = true
          } //暫時只考慮L於W交換
        else if (avBox.boxType == 0) {
          avBox.boxType = 1
          avBox.changeLW()
          if (isFeasiblePack(avBox, sp, placedBox)){
            blockList += avBox
            flag = true
          }
        }
      }
    }

    /**
      * 判斷當前塊是否可行放置
      */
    def isFeasiblePack(b: Box, s: Position, placedBox: ArrayBuffer[SpaceBox]): Boolean = {
      if (Common.more(b.l + s.x, carL) || Common.more(b.w + s.y, carW) || Common.more(b.h + s.z, carH)) return false
      //長度限制
      var area: Double = 0.0 //橫截面
      for (i <- s.x.toInt+1 to s.x.toInt + b.l.toInt)
        for (j <- s.y.toInt+1 to s.y.toInt + b.w.toInt)
          for (k <- s.z.toInt to s.z.toInt)
            if (matrix3D(i)(j)(k)) area += 1
      if(!Common.eq(s.z, 0) && Common.less(area / b.area, Percent)) return false
      for (i <- s.x.toInt+1  to s.x.toInt + b.l.toInt-1)
        for (j <- s.y.toInt+1  to s.y.toInt + b.w.toInt-1)
          for (k <- s.z.toInt+1 to s.z.toInt + b.h.toInt-1) {
            if (matrix3D(i)(j)(k)) return false
          }
      if (s.fragility && !b.fragility) return false //易碎約束
      if (!feasiblePandD(b, s, placedBox)) return false //pick and Delivery LIFO
      true
    }

    /**
      * pick and delivery LIFO
      */
    def feasiblePandD(b: Box, s: Position, placedBox: ArrayBuffer[SpaceBox]): Boolean = {
      for (pb <- placedBox) {
        if ((Common.ge(s.x, pb.space.x + pb.block.l) && ((Common.more(s.y + b.w, pb.space.y) &&
          Common.ge(pb.space.y + pb.block.w, s.y + b.w)) || (Common.more(pb.space.y + pb.block.w, s.y) && Common.ge(s.y, pb.space.y)) ||
          (Common.ge(pb.space.y, s.y) && Common.ge(s.y + b.w, pb.space.y + pb.block.w))) && Common.more(s.z + b.h, pb.space.z))
          || (Common.ge(s.z, pb.space.z + pb.block.h) && ((Common.more(s.y + b.w, pb.space.y) &&
          Common.ge(pb.space.y + pb.block.w, s.y + b.w)) || (Common.more(pb.space.y + pb.block.w, s.y) && Common.ge(s.y, pb.space.y)) ||
          (Common.ge(pb.space.y, s.y) && Common.ge(s.y + b.w, pb.space.y + pb.block.w))) && ((Common.more(s.x + b.l, pb.space.x) &&
          Common.ge(pb.space.x + pb.block.l, s.x + b.l)) || (Common.more(pb.space.x + pb.block.l, s.x) && Common.ge(s.x, pb.space.x)) ||
          (Common.ge(pb.space.x, s.x) && Common.ge(s.x + b.l, pb.space.x + pb.block.l))))) {
          {
            if (b.inoder < pb.block.inoder || b.outorder > pb.block.outorder) return false
          }
        }
        if((Common.le(s.x+b.l, pb.space.x)&&((Common.more(s.y+b.w, pb.space.y) &&
          Common.ge(pb.space.y+pb.block.w, s.y+b.w)) || (Common.more(pb.space.y+pb.block.w, s.y) && Common.ge(s.y, pb.space.y)) ||
          (Common.ge(pb.space.y, s.y) && Common.ge(s.y+b.w, pb.space.y+pb.block.w))) && Common.more(pb.space.z+pb.block.h,s.z))
          || (Common.le(s.z+b.h,pb.space.z) && ((Common.more(s.y+b.w, pb.space.y) &&
          Common.ge(pb.space.y+pb.block.w, s.y+b.w)) || (Common.more(pb.space.y+pb.block.w, s.y) && Common.ge(s.y, pb.space.y)) ||
          (Common.ge(pb.space.y, s.y) && Common.ge(s.y+b.w, pb.space.y+pb.block.w))) && ((Common.more(s.x+b.l, pb.space.x) &&
          Common.ge(pb.space.x+pb.block.l, s.x+b.l)) || (Common.more(pb.space.x+pb.block.l, s.x) && Common.ge(s.x, pb.space.x)) ||
          (Common.ge(pb.space.x, s.x) && Common.ge(s.x+b.l, pb.space.x+pb.block.l))))){
            if(b.inoder > pb.block.inoder || b.outorder < pb.block.outorder) return false
        }
      }
      true
    }

    //check 沒有考慮LIFO的檢驗
    def checkPlan(): Boolean = {
      if (plan.length > boxList.length) false //計劃裝載的箱子數不會大於總箱子數
      else if (planVolume > (carL * carH * carW)) false //計劃裝載總體積不會大於車廂總體積
      else {
        for (b1 <- plan) {
          for (b2 <- plan if (b1 != b2)) {
            //由於算法原因沒有考慮b1.x < b2.x && b1.x +b1.lx > b2.x + b2.lx
            if (((b2.space.x < b1.space.x && b1.space.x < b2.space.x + b2.block.l) ||
              ((b2.space.x - b1.space.x).abs <= 1e-6) ||
              (b2.space.x > b1.space.x && b1.space.x + b1.block.l > b2.space.x)) &&
              ((b1.space.y > b2.space.y && b1.space.y < b2.space.y + b2.block.w) ||
                ((b2.space.y - b1.space.y).abs <= 1e-6) ||
                (b1.space.y < b2.space.y && b1.space.y + b1.block.w > b2.space.y)) &&
              ((b1.space.z > b2.space.z && b1.space.z < b2.space.z + b2.block.h) ||
                ((b1.space.z - b2.space.z) <= 1e-6) ||
                (b1.space.z < b2.space.z && b1.space.z + b1.block.h > b2.space.z))) false
            else if (b1.space.x + b1.block.l > carL || b1.space.y + b1.block.w > carW || b1.space.z + b1.block.h > carH) false

          }
        }
        //   println("-------------------check normal no problem-------------------")
        true
      }
    }

  }
