package tlpdp.loadFeasible

import java.io.FileWriter

import tlpdp.common.Common
import tlpdp.model.{Box, Position, SpaceBox}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


/**
  * Created by hc on 16-12-15.
  */
//basicHeuristic of loading
class BasicHeuristic(
  var ps : Array[Int],          //loading order of box
  val container : Position,      //(0,0,0)+存放空間長寬高
  var boxList : ArrayBuffer[Box]  //box list
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

  //add container
  var avail : ArrayBuffer[Box] = new ArrayBuffer[Box]()
  //copy boxList/available box
  var blockList : ArrayBuffer[Box] = new ArrayBuffer[Box]()
  //available box for current space
  val areaPercent : Double = Double.PositiveInfinity
  //穩定性約束比例 未定
  /**
    * basicHeuristic main process
    */
  var blockFromPs : Box = new Box()
  var spaceFromStack : Position = new Position()

  val Percent : Double = 0.75

  /**
    * genBlockList (space,avail)   output blockList      //genenrate available boxes in current space
    */
  def genBlockList(sp : Position, placedBox : ArrayBuffer[SpaceBox]):Unit = {
    for (avBox <- avail){
      if(isFeasiblePack(avBox,sp, placedBox)) blockList += avBox              //暫時只考慮L於W交換
      else if(avBox.boxType == 0) {
        avBox.boxType = 1
        avBox.changeLW()
        if (isFeasiblePack(avBox, sp, placedBox)) blockList += avBox
/*        else if (avBox.boxType == 1) {
          avBox.boxType = 2
          avBox.changeLW() // 先換成原位
          avBox.changeLH()
          if (isFeasiblePack(avBox, sp, placedBox)) blockList += avBox
        else if(avBox.boxType ==2){
            avBox.boxType = 3
            avBox.changeWH() // 先換成原位
            avBox.changeLH()
            if (isFeasiblePack(avBox, sp, placedBox)) blockList += avBox
            else if(avBox.boxType==3){
              avBox.boxType = 4
              avBox.changeLH() // 先換成原位
              avBox.changeHLW()
              if (isFeasiblePack(avBox, sp, placedBox)) blockList += avBox
              else if(avBox.boxType==4){
                avBox.boxType=5
                avBox.changeHLW()
                avBox.changeWHL()
              }
            }
          }
        }*/
      }
    }
  }

  /**
    * 判斷當前塊是否可行放置
    */
  def isFeasiblePack(b : Box, s : Position, placedBox : ArrayBuffer[SpaceBox]):Boolean ={
    if(Common.more(b.l,s.lx) || Common.more(b.w , s.ly) || Common.more(b.h,s.lz)) return false     //長度限制
//  else if( !Common.eq(s.z, 0) && Common.more(b.area / s.area , areaPercent)) false        //支撐面限制
    else if(s.fragility && !b.fragility) return false         //易碎約束
    else if(!feasiblePandD(b,s,placedBox))  return false             //pick and Delivery LIFO
    else true
  }

  /**
    * pick and delivery LIFO
    */
  def feasiblePandD(b : Box, s : Position, placedBox : ArrayBuffer[SpaceBox]): Boolean = {
    for(pb <- placedBox){
      if((Common.ge(s.x, pb.space.x + pb.block.l)&&((Common.more(s.y+b.w, pb.space.y) &&
        Common.ge(pb.space.y+pb.block.w, s.y+b.w)) || (Common.more(pb.space.y+pb.block.w, s.y) && Common.ge(s.y, pb.space.y)) ||
        (Common.ge(pb.space.y, s.y) && Common.ge(s.y+b.w, pb.space.y+pb.block.w))) && Common.more(s.z+b.h, pb.space.z))
        || (Common.ge(s.z,pb.space.z+pb.block.h) && ((Common.more(s.y+b.w, pb.space.y) &&
        Common.ge(pb.space.y+pb.block.w, s.y+b.w)) || (Common.more(pb.space.y+pb.block.w, s.y) && Common.ge(s.y, pb.space.y)) ||
        (Common.ge(pb.space.y, s.y) && Common.ge(s.y+b.w, pb.space.y+pb.block.w))) && ((Common.more(s.x+b.l, pb.space.x) &&
        Common.ge(pb.space.x+pb.block.l, s.x+b.l)) || (Common.more(pb.space.x+pb.block.l, s.x) && Common.ge(s.x, pb.space.x)) ||
        (Common.ge(pb.space.x, s.x) && Common.ge(s.x+b.l, pb.space.x+pb.block.l))))){
        if((b.id==0 && pb.block.id == 0) || (b.id == -1 && pb.block.id == -1)){
          if(b.inoder < pb.block.inoder || b.inoder1 < pb.block.inoder || b.inoder < pb.block.inoder1 || b.inoder1 < pb.block.inoder1 || b.outorder > pb.block.outorder ||
            b.outorder1 > pb.block.outorder || b.outorder > pb.block.outorder1 || b.outorder1 > pb.block.outorder1 ) return false
        }else if(b.id == 0 || b.id == -1){
          if(b.inoder < pb.block.inoder || b.inoder1 < pb.block.inoder || b.outorder > pb.block.outorder ||
            b.outorder1 > pb.block.outorder) return false
        }else if(pb.block.id == 0 || pb.block.id == -1) {
          if (b.inoder < pb.block.inoder || b.inoder < pb.block.inoder1 || b.outorder > pb.block.outorder ||
            b.outorder > pb.block.outorder1) return false
        }else{
          if(b.inoder < pb.block.inoder || b.outorder > pb.block.outorder) return false
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
        if((b.id == 0 && pb.block.id == 0)){
          if(b.inoder > pb.block.inoder || b.inoder1 > pb.block.inoder || b.inoder > pb.block.inoder1 || b.inoder1 > pb.block.inoder1 || b.outorder < pb.block.outorder ||
            b.outorder1 < pb.block.outorder || b.outorder < pb.block.outorder1 || b.outorder1 < pb.block.outorder1 ) return false
        }else if(b.id == 0){
          if(b.inoder > pb.block.inoder || b.inoder1 > pb.block.inoder || b.outorder < pb.block.outorder ||
            b.outorder1 < pb.block.outorder) return false
        }else if(pb.block.id == 0) {
          if (b.inoder > pb.block.inoder || b.inoder > pb.block.inoder1 || b.outorder < pb.block.outorder ||
            b.outorder < pb.block.outorder1) return false
        }else{
          if(b.inoder > pb.block.inoder || b.outorder < pb.block.outorder) return false
        }
      }
    }
    true
  }

  /**
    * genResidulSpace(space,box) output space
    */
  def genResidulSpace(space : Position, block : Box, maxlen : Double):Unit = {
    var newPosition1 : Position = new Position(space.x+block.l, space.y, space.z)
    var newPosition2 : Position = new Position(space.x, space.y+block.w, space.z)
   /* if(block.id == -1) {
      newPosition2.y = space.y + block.maxW
    }*/
    var newPosition3 : Position = new Position(space.x, space.y, space.z+block.h)
    if(block.id ==0){
      val upBox = boxList.filter(_.id == block.combineUpId)          //上邊頂點的可用長寬等於放於上邊box的長寬
      newPosition3.lx = upBox(0).l
      newPosition3.ly = upBox(0).w
      newPosition3.lz = space.lz - block.h
    }
    else{
      newPosition3.lx = block.l
      newPosition3.ly = block.w
      newPosition3.lz = space.lz - block.h
    }
    if(block.fragility) newPosition3.fragility = true
    if(space.fragility){
      newPosition2.fragility =true
      newPosition1.fragility = true
    }
    if(Common.more(space.ly,maxlen)&&Common.more(maxlen,space.ly - block.w)&&Common.more(maxlen,space.lx)&&Common.less(block.w,maxlen)){        //判斷放置點的剩餘空間中是否能夠裝得下剩餘的物品
      newPosition1.lx = space.lx - block.l
      newPosition1.ly = space.ly
      newPosition1.lz = space.lz
      newPosition2.lx = block.l
      newPosition2.ly = space.ly - block.w
      newPosition2.lz = space.lz
      spaceStack.insert(0,newPosition3, newPosition1, newPosition2)
    }
    else if(Common.more(space.lx,maxlen)&&Common.more(maxlen,space.lx - block.l)&&Common.more(maxlen,space.ly)&&Common.less(block.l,maxlen)){
      newPosition1.lx = space.lx - block.l
      newPosition1.ly = block.w
      newPosition1.lz = space.lz
      newPosition2.lx = space.lx
      newPosition2.ly = space.ly - block.w
      newPosition2.lz = space.lz
      spaceStack.insert(0,newPosition3, newPosition2, newPosition1)
    }
    else if(remainSpace(space.lx-block.l,space.ly-block.w))     //如果兩個放置點都放的下，則比較mx於my
    {       //compare mx and my               //暫時除去mx及my
      newPosition1.lx = space.lx - block.l
      newPosition1.ly = space.ly
      newPosition1.lz = space.lz
      newPosition2.lx = block.l
      newPosition2.ly = space.ly - block.w
      newPosition2.lz = space.lz
      spaceStack.insert(0,newPosition3, newPosition1, newPosition2)
    }
    else{
      newPosition1.lx = space.lx - block.l
      newPosition1.ly = block.w
      newPosition1.lz = space.lz
      newPosition2.lx = space.lx
      newPosition2.ly = space.ly - block.w
      newPosition2.lz = space.lz
      spaceStack.insert(0,newPosition3, newPosition2, newPosition1)
    }
    removeRepeat()        //去除重復點

  }

  /**
    * 去除重復可行點，排除position相同，可行放置域不同的點
    */
  def removeRepeat (): Unit = {
    val copy : ArrayBuffer[Position] = copySpack(spaceStack)
    for(i <- 0 until copy.length){
      for(j <- i+1 until copy.length){
        if(Common.eq(copy(i).x,copy(j).x) && Common.eq(copy(i).y , copy(j).y) && Common.eq(copy(i).z , copy(j).z) &&
          ( !Common.eq(copy(i).lx , copy(j).lx) || !Common.eq(copy(i).ly , copy(j).ly) || !Common.eq(copy(i).lz , copy(j).lz) )){
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
  def copySpack (sp : ArrayBuffer[Position]) : ArrayBuffer[Position] = {
    var copy : ArrayBuffer[Position] = new ArrayBuffer[Position]()
    copy ++= sp
  }
  /**
    * 剩餘空間劃分,判斷mx於my大小 大於
    */
  def remainSpace (mx : Double, my : Double): Boolean =
    mx > my && (mx-my).abs > 1e-6

  /**
    * transferSpace(space,spaceStack)    //轉移剩餘空間
    */
  def transferSpace(space : Position):Unit={
     val p : Position = if(!spaceStack.isEmpty) spaceStack(0) else return
      if(Common.eq(p.x + p.lx,space.x) && Common.more(space.y + space.ly , p.y) &&Common.less(space.y,p.y)&& Common.eq(p.z , space.z) ){       //將可行空間轉移給新的棧頂元素(只考慮放置一個矩形塊產生的三塊空間內的空間轉移）
        p.lx += space.lx
      }else if(Common.eq(p.y + p.ly , space.y) && Common.more(space.x + space.lx , p.x) && Common.less(space.x,p.x)&& Common.eq(p.z , space.z) ){
        p.ly += space.ly
      }

  }

  def basicHeuristic():Boolean = {
    boxList = boxList.sortWith(_.id < _.id)    //當area相等時按id排序
    if(basicHeuristic(boxList,1)) return true
    else{
      var comBox = combine(boxList,avail)      //只針對不可行塊的結合
      comBox = comBox.sortWith(_.id < _.id)
      if(!comBox.equals(boxList)&&basicHeuristic(comBox,1)) return true
      else{
        var comBox1 = combine_two(boxList)
        comBox1 = comBox1.sortWith(_.id < _.id)
        if(!comBox1.equals(boxList))
          if(basicHeuristic(comBox1)) return true
     /*   val comBox2 = combine_three(boxList)
        if(!comBox2.equals(boxList))
          if(basicHeuristic(comBox2)) return true*/
      }
      //      false
    }
    return false
    /*-----------------------*/
//    val comBox1 = combine_two(boxList)                  //採用先結合（只結合一組）後裝載的方式
//    if(basicHeuristic(comBox1)) true else false
  }

  def reset():Unit = {
    spaceStack.clear()
    spaceStack += container
    plan.clear()
    planVolume = 0.0
  }
  def basicHeuristic(bo : ArrayBuffer[Box]): Boolean = {
    reset()
    var index : Int = 0   //index in ps
    val len : Int = bo.length
//    avail = bo.sortWith(_.l > _.l)

//    avail = bo.sortWith(_.fragility < _.fragility)                        //sort by box volume
//    avail = bo.sortWith(_.boxVolume > _.boxVolume)                  //    易碎排後邊
    avail = bo.sortWith(_.area > _.area)
    avail = avail.sortWith(_.fragility < _.fragility)

    while(!isSpaceEmpty() && avail.nonEmpty){
      spaceFromStack = spaceStack(0)   //get top of stack
      for(b <- avail) b.resetDirection()
      blockList.clear()                   //清空列表中元素
      genBlockList(spaceFromStack, plan)         //update blocklist
      spaceStack.remove(0)        //delete the first one of stack
      if(!isBlockListEmpty()){
        if(index >=len) index=0
        blockFromPs = blockList(psMode(ps(index)))
        availRemove(blockFromPs)
        index += 1
        if(blockFromPs.id==0){               //如果爲組合塊
          val downBox = boxList.filter(_.id == blockFromPs.combineDownId)
          val upBox = boxList.filter(_.id == blockFromPs.combineUpId)
          val upPosition = new Position(spaceFromStack.x,spaceFromStack.y,spaceFromStack.z+downBox(0).h)
          if(blockFromPs.boxType ==1){
            downBox(0).changeLW()
            upBox(0).changeLW()
            downBox(0).boxType =1
            upBox(0).boxType =1
          }
          plan += new SpaceBox(spaceFromStack,downBox(0))
          plan += new SpaceBox(upPosition,upBox(0))
          planVolume = planVolume + upBox(0).boxVolume + downBox(0).boxVolume
        }else{
          plan += new SpaceBox(spaceFromStack, blockFromPs)
          planVolume += blockFromPs.boxVolume
        }
        val maxLen : Double = getMaxLen()
        genResidulSpace(spaceFromStack, blockFromPs,maxLen)   //更新spacestack空間
//        combinePosition()
      }else{
        transferSpace(spaceFromStack)           //轉移剩餘空間
      }

    }
    if (avail.isEmpty && checkPlan()) true else false                      //測試test不可能所有箱子都能裝載(需要把avail。isEmpty注釋）

  }

  def basicHeuristic(bo : ArrayBuffer[Box], f : Int): Boolean = {
    reset()
    var index : Int = 0   //index in ps
    val len : Int = bo.length
//    avail = bo.sortWith(_.fragility < _.fragility)                        //sort by box volume
//    avail = avail.sortWith(_.boxVolume > _.boxVolume)                  //    易碎排後邊

//    avail = avail.sortWith(_.boxVolume > _.boxVolume)
    avail = bo.sortWith(_.area > _.area)
    avail = avail.sortWith(_.fragility < _.fragility)
//    avail = avail.sortWith(_.l > _.l)
    while(!isSpaceEmpty() && avail.nonEmpty){
      spaceFromStack = spaceStack(0)   //get top of stack
      for(b <- avail) b.resetDirection()
      blockList.clear()                   //清空列表中元素
      genBlockList(spaceFromStack, plan)         //update blocklist
      spaceStack.remove(0)        //delete the first one of stack
      if(!isBlockListEmpty()){
        if(index >=len) index=0
        blockFromPs = blockList(psMode(ps(index)))
        availRemove(blockFromPs)
        index += 1
        if(blockFromPs.id==0){               //如果爲組合塊
        val downBox = boxList.filter(_.id == blockFromPs.combineDownId)
          val upBox = boxList.filter(_.id == blockFromPs.combineUpId)
          val upPosition = new Position(spaceFromStack.x,spaceFromStack.y,spaceFromStack.z+downBox(0).h)
          if(blockFromPs.boxType ==1){
            downBox(0).changeLW()
            upBox(0).changeLW()
            downBox(0).boxType =1
            upBox(0).boxType =1
          }
          plan += new SpaceBox(spaceFromStack,downBox(0))
          plan += new SpaceBox(upPosition,upBox(0))
          planVolume = planVolume + upBox(0).boxVolume + downBox(0).boxVolume
        } else{
          plan += new SpaceBox(spaceFromStack, blockFromPs)
          planVolume += blockFromPs.boxVolume
        }
        val maxLen : Double = getMaxLen()
        genResidulSpace(spaceFromStack, blockFromPs,maxLen)   //更新spacestack空間
        //        combinePosition()
      }else{
        transferSpace(spaceFromStack)           //轉移剩餘空間
      }

    }
    if (avail.isEmpty && checkPlan()) true    //測試test不可能所有箱子都能裝載(需要把avail。isEmpty注釋）
    else basicHeuristic(bo)

  }

  /*
  def combinePosition():Unit = {
    if(spaceStack.length > 3){
      for(s <- spaceStack.length-1 to spaceStack.length-3 by -1){
        for(p <- 0 to spaceStack.length-4){
          if(spaceStack(s).z > 0 && Common.eq(spaceStack(s).z, spaceStack(p).z) && Common.eq(spaceStack(s).x,spaceStack(p).x)
            && Common.le(spaceStack(s).ly,spaceStack(p).ly) && Common.eq(spaceStack(p).x + spaceStack(p).lx, spaceStack(s).x)){
            spaceStack(p).lx += spaceStack(s).lx
            spaceStack(p).ly = spaceStack(s).ly
            if(spaceStack(s).fragility) spaceStack(p).fragility = true
            spaceStack.remove(s)
          }
          if(spaceStack(s).z > 0 && Common.eq(spaceStack(s).z, spaceStack(p).z) && Common.eq(spaceStack(s).y,spaceStack(p).y)
            && Common.le(spaceStack(s).lx,spaceStack(p).lx) && Common.eq(spaceStack(p).y + spaceStack(p).ly, spaceStack(s).y)){
            spaceStack(p).ly += spaceStack(s).ly
            spaceStack(p).lx = spaceStack(s).lx
            if(spaceStack(s).fragility) spaceStack(p).fragility = true
            spaceStack.remove(s)
          }
        }
      }
    }
  }*/
//  def max(a: Double,b: Double)=if(Common.ge(a,b)) a else b

  /**
    * 塊結合（將不可行塊放於其他塊上方，結合成新塊  unpack 在上方 removeUnpack 在下方
    */

  def combine(bl :ArrayBuffer[Box], unpack :ArrayBuffer[Box]):ArrayBuffer[Box]={
    for(b <- bl) b.resetDirection()                          //先將箱子初始化
    //    for(b <- unpack) b.resetDirection()
    var removeUnpackBox = bl.filter(!unpack.contains(_))
    var newBoxArray : ArrayBuffer[Box] = removeUnpackBox.clone()
    var isFragile : Boolean = false
    for(un <- unpack){
      if(un.fragility) isFragile = true else isFragile = false
      removeUnpackBox = removeUnpackBox.sortWith(_.absDiffer(un.area) < _.absDiffer(un.area))
      var flag : Boolean = true
      var rid : Int = -1                //save combine box id
      for(r <- removeUnpackBox if flag){
        if((Common.ge(math.min(r.l,un.l)*math.min(r.w,un.w) / un.area , Percent) || Common.ge(math.min(r.l,un.l)*math.min(r.w,un.w) /r.area, Percent))&& Common.le(r.h+un.h,carH)) {
          if((isFragile || (!isFragile&&(!r.fragility)))&& (r.outorder>=un.outorder)&&(r.inoder<=un.inoder) && Common.ge(math.min(r.l,un.l)*math.min(r.w,un.w) / un.area , Percent)){
            /*  if(Common.more(math.abs(un.l-r.l),math.abs(un.l-r.w))){
                r.changeLW()
                r.boxType =1
              }*/
            rid = r.id
            val newBox = genNewBox(un,r)
            newBoxArray = newBoxArray.filter(_.id != r.id)
            newBoxArray += newBox
            flag = false
          }else if(!isFragile && r.fragility && (r.outorder<=un.outorder)&&(r.inoder>=un.inoder) && Common.ge(math.min(r.l,un.l)*math.min(r.w,un.w) /r.area, Percent)){
            /*   if(Common.more(math.abs(un.l-r.l),math.abs(un.l-r.w))){
                 r.changeLW()
                 r.boxType =1
               }*/
            rid = r.id
            val newBox = genNewBox(r,un)
            newBoxArray = newBoxArray.filter(_.id != r.id)
            newBoxArray += newBox
            flag = false
          }
        }
      }
      if(flag) newBoxArray += un
      else if(rid>=0) removeUnpackBox = removeUnpackBox.filter(_.id != rid)
    }
    newBoxArray
  }

  /**
    * 上下生成新組合塊
    * @param up
    * @param down
    * @return
    */
  def genNewBox(up : Box , down : Box) : Box = {
    val newBox : Box = new Box(up.h+down.h,math.max(up.w,down.w),math.max(up.l,down.l),up.fragility,0)
    newBox.combineUpId = up.id
    newBox.combineDownId = down.id
//    newBox.inoder = math.max(up.inoder,down.inoder)
//    newBox.outorder = math.min(up.outorder,down.outorder)
    newBox.inoder = up.inoder
    newBox.outorder = up.outorder
    newBox.inoder1 = down.inoder
    newBox.outorder1 = down.outorder
    newBox
  }

  /*
    前後生成新組合塊
   */
  def genNewBox1(front : Box , back : Box):Box = {
    val newBox : Box = new Box(front.h, back.w, front.l + back.l, if(front.fragility || back.fragility) true else false, -1)
    newBox.combineUpId = front.id         //front
    newBox.combineDownId = back.id        //back
    //    newBox.inoder = math.max(up.inoder,down.inoder)
    //    newBox.outorder = math.min(up.outorder,down.outorder)
    newBox.inoder = front.inoder
    newBox.outorder = front.outorder
    newBox.inoder1 = back.inoder
    newBox.outorder1 = back.outorder
    //newBox.maxW = back.w
    newBox
  }

  class areaPair(val r : Int, val l : Int, val cost : Double,val flag : Boolean) extends Ordered[areaPair] {
    def compare(that: areaPair): Int = {
      //按cost 順序
      val co = this.cost - that.cost
      if (co.abs <= 1e-6) 0
      else if (co > 0) 1
      else -1
    }
  }

  /**
    * 塊結合策略 ，針對所有的box，選擇area最接近的兩個box進行結合
    */
  def combine_two(bl : ArrayBuffer[Box]):ArrayBuffer[Box] = {
    for(b <- bl) b.resetDirection()                          //先將箱子初始化
    var newBoxArray : ArrayBuffer[Box] = bl.clone()
    val boxOrderByArea = bl.sortWith(_.area > _.area)
    var Pairs : Array[areaPair] = (                   //在滿足支撐面積的前提下，按長，寬差值和從小達到排序
      for (i <- 0 until boxOrderByArea.length-1) yield {
        val commonArea = math.min(boxOrderByArea(i).l,boxOrderByArea(i+1).l)*math.min(boxOrderByArea(i).w,boxOrderByArea(i+1).w)
        if(Common.ge(commonArea/boxOrderByArea(i+1).area , Percent)&& Common.ge(commonArea/boxOrderByArea(i).area, Percent))
        //           new areaPair(i , i+1 , math.abs(boxOrderByArea(i+1).area-boxOrderByArea(i).area),true)
          new areaPair(i , i+1 , math.abs(boxOrderByArea(i+1).l-boxOrderByArea(i).l)+math.abs(boxOrderByArea(i+1).w-boxOrderByArea(i).w),true)
        else
        //            new areaPair(i , i+1 , math.abs(boxOrderByArea(i+1).area-boxOrderByArea(i).area),false)
          new areaPair(i , i+1 , math.abs(boxOrderByArea(i+1).l-boxOrderByArea(i).l)+math.abs(boxOrderByArea(i+1).w-boxOrderByArea(i).w),false)
      }).toArray
    Pairs = Pairs.sorted
    Pairs = Pairs.filter(_.flag)
    if(!Pairs.isEmpty){
      val b1 = boxOrderByArea(Pairs(0).l)
      val b2 = boxOrderByArea(Pairs(0).r)
      var isFragile : Boolean = false
      if(b1.fragility) isFragile = true else isFragile = false
      if(Common.le(b1.h+b2.h,carH)){
        if ((isFragile || (!isFragile && (!b2.fragility)))&&(b1.outorder<=b2.outorder)&&(b1.inoder>=b2.inoder)) {
          /*  if (Common.more(math.abs(b1.l - b2.l), math.abs(b1.l - b2.w))) {
              b1.changeLW()
              b1.boxType = 1
            }*/
          val newBox = genNewBox(b1,b2)
          newBoxArray = newBoxArray.filter(_.id != b1.id)
          newBoxArray = newBoxArray.filter(_.id != b2.id)
          newBoxArray += newBox
        }else if(!isFragile && b2.fragility && (b1.outorder>=b2.outorder)&&(b1.inoder<=b2.inoder)){
          /*  if (Common.more(math.abs(b1.l - b2.l), math.abs(b1.l - b2.w))) {
              b1.changeLW()
              b1.boxType = 1
            }*/
          val newBox = genNewBox(b2,b1)
          newBoxArray = newBoxArray.filter(_.id != b1.id)
          newBoxArray = newBoxArray.filter(_.id != b2.id)
          newBoxArray += newBox
        }
      }
    }
    newBoxArray

  }

  /**
    * 塊結合策略，前後結合
    */
  def combine_three(bl : ArrayBuffer[Box]):ArrayBuffer[Box] = {
    for(b <- bl) b.resetDirection()                          //先將箱子初始化
    var newBoxArray : ArrayBuffer[Box] = bl.clone()
    val boxOrderByW = bl.sortWith(_.w > _.w)
    val boxOrderByH = boxOrderByW.sortWith(_.h > _.h)
    var flag : Boolean = true
    for(b <- 0 until boxOrderByW.length-1 if flag){
      if(Common.eq(boxOrderByW(b).h, boxOrderByW(b+1).h)){
        val b1 = boxOrderByW(b)        //b1 放在裏邊
        val b2 = boxOrderByW(b+1)
        if(Common.le(((b1.w - b2.w)*b2.l)/((b1.l+b2.l)*b1.w), 0.25) && Common.le((b1.l + b2.l), carL) && (b1.outorder>=b2.outorder)&&(b1.inoder<=b2.inoder)){
          val newBox = genNewBox1(b2,b1)
          newBoxArray = newBoxArray.filter(_.id != b1.id)
          newBoxArray = newBoxArray.filter(_.id != b2.id)
          newBoxArray += newBox
          flag = false
        }
      }
    }
    newBoxArray
  }
  /**
    * 獲取剩餘未裝箱子的三邊長度的最大值
    */
  def getMaxLen() : Double = {
    var maxlength : Double = 0.0
    for(b <- avail){
      if(Common.more(b.maxLine,maxlength)) maxlength = b.maxLine
    }
    maxlength
  }
  /**
    * 判斷可選放置空間是否爲空
    */
  def isSpaceEmpty(): Boolean = if (spaceStack.length == 0) true else false

  /**
    * 判斷生成的可行裝載塊是否爲空
    */
  def isBlockListEmpty(): Boolean = if(blockList.length == 0) true else false

  //if ps > blockList.length 對ps取模
  def psMode(psIndex : Int):Int={
    if(blockList.length > psIndex) psIndex else psIndex % blockList.length
  }

  //remove current box from avai
  def availRemove(removeBlock : Box): Unit ={
    var flag : Boolean = false
      for(b  <- 0 until avail.length if !flag){
        if(avail(b).id == removeBlock.id && avail(b).combineUpId == removeBlock.combineUpId){
          avail.remove(b)
          flag = true
        }
      }
  }

  //check 沒有考慮LIFO的檢驗
  def checkPlan() : Boolean = {
    if(plan.length > boxList.length) false                 //計劃裝載的箱子數不會大於總箱子數
    else if(planVolume > (carL * carH * carW)) false       //計劃裝載總體積不會大於車廂總體積
    else{
      for(b1 <- plan){
        for(b2 <- plan if(b1!=b2)){                           //由於算法原因沒有考慮b1.x < b2.x && b1.x +b1.lx > b2.x + b2.lx
          if(((b2.space.x < b1.space.x && b1.space.x < b2.space.x + b2.block.l)||
            ((b2.space.x - b1.space.x).abs <= 1e-6)||
            (b2.space.x > b1.space.x && b1.space.x + b1.block.l > b2.space.x))&&
            ((b1.space.y > b2.space.y && b1.space.y < b2.space.y + b2.block.w) ||
              ((b2.space.y - b1.space.y).abs <= 1e-6) ||
              (b1.space.y < b2.space.y && b1.space.y + b1.block.w > b2.space.y)) &&
            ((b1.space.z > b2.space.z && b1.space.z < b2.space.z+b2.block.h) ||
              ((b1.space.z - b2.space.z) <= 1e-6) ||
              (b1.space.z < b2.space.z && b1.space.z + b1.block.h > b2.space.z))) false
          else if(b1.space.x + b1.block.l > carL || b1.space.y + b1.block.w > carW || b1.space.z + b1.block.h > carH) false

        }
      }
 //   println("-------------------check normal no problem-------------------")
      true
    }
  }

}

object  BasicHeuristic {

  def main(args : Array[String]):Unit= {
      var oneLineData : Array[Double] = new Array(0)
      println("---------------FILE read start------------------")
      val out = new FileWriter("/home/hc/IdeaProjects/result/loading_final", true)
      out.write("------------------LOADING RESULT-----------------\n")
//      out.write("BR1\t\t\tBR2\t\t\tBR3\t\t\tBR4\t\t\tBR5\t\t\tBR6\t\t\tBR7\t\t\tBR8\t\t\tBR9\t\t\tBR10\t\t\tBR11\t\t\tBR12\t\t\tBR13\t\t\tBR14\t\t\tBR15\n")
      out.write("BR8\t\t\tBR9\t\t\tBR10\t\t\tBR11\t\t\tBR12\t\t\tBR13\t\t\tBR14\t\t\tBR15\n")
      for(i <- 8 to 15){//測試集0-15(8-15 強異構測試集）
        val str = f"BR1-15/thpack$i%d.txt"
        var totalRate : Double = 0.0
        println("Reading data: " + str)
        val lineIte : Iterator[String] = Source.fromFile(str).getLines()
        oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)
        val sumTest : Int = oneLineData(0).toInt
        for(j <- 1 to sumTest){
          //        lineIte.next()
          lineIte.next()
          var origin : Position = new Position()    //創建原點坐標
          oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)
          origin.lx = oneLineData(0)
          origin.ly = oneLineData(1)
          origin.lz = oneLineData(2)

          //box
          oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)
          val boxType = oneLineData(0)    //box type
          var boxes : ArrayBuffer[Box] = new ArrayBuffer[Box]()           //total boxes
          var totalBoxes: Int = 0
          var id : Int =0
          for(ite <- 0 until boxType.toInt){
            oneLineData = lineIte.next().split("\\s+").filter(!_.isEmpty()).map(_.toDouble)
            for(i <- 0 until oneLineData(7).toInt){
              boxes += new Box(oneLineData(5),oneLineData(3),oneLineData(1),false,id)
              id = id +1
            }
            totalBoxes += oneLineData(7).toInt

          }
          val ps : Array[Int] = new Array[Int](totalBoxes)
//          val ps : Array[Int] = Range(0,totalBoxes).toArray
          val bh = new BasicHeuristic(ps,origin,boxes)
          var rate : Double = 0.0
          if(bh.basicHeuristic(boxes)){
            println("-------------successful-----------------")
            println("------------result ----------------------------" + j)
            for (pl <- bh.plan){
              println(pl)
            }
            rate = bh.planVolume * 100/ (bh.carL *bh.carH * bh.carW)
            println("length:"+bh.plan.length)
            println(rate)
            totalRate += rate

          }else{
            println("------------fail----------------")
          }
        }
        val avgRate : Double = totalRate / sumTest
        println("avgRate: " + avgRate)
        out.write(avgRate+"\t\t\t")
      }
    out.close()
  }
}