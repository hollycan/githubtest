package tlpdp.model

import tlpdp.common.Common

/**
  * Created by hc on 16-12-15.
  */
class Box(

    var h : Double = 1.0,               //爲了能夠交換朝向，改爲可變變量
    var w : Double = 1.0,
    var l : Double = 1.0,
    val fragility : Boolean = false,
    val id : Int = 1              //id 0 combine box

)extends Serializable {

    var pos : Position = new Position()
    var boxVolume = h * w * l            //the volume of box
    var area : Double = w * l            //底面積
    var boxType : Int = 0                //表示箱子的朝向 0:正常朝向 1：h不變，l，w交換 2：l不變，w，h交換  3:w 不變，l，h交換

    var inoder: Int = 0               //裝箱的順序(根據在vehicle中的位置來賦值）
    var outorder: Int = 0             //卸載的順序
//    def maxLine : Double = if(Common.ge(l,w)&&Common.ge(l,h)) l else if(Common.ge(w,l)&&Common.ge(w,h)) w else h
    var combineUpId : Int = 0       //若有結合塊，放於上方的box
    var combineDownId : Int = 0        //結合塊放於下方的box

    var inoder1 : Int = 0       //combine box
    var outorder1: Int = 0
    var maxW : Double = 0.0

    var fitness : Double = 0.0

    def maxLine : Double = if(Common.ge(l,w)) l else w     //先考慮h不變，只交換l，w的情況

    def absDiffer(a : Double):Double={
        math.abs(this.area-a)
    }

/*    override def compare(that : Box) : Int = {
        //按底面積從大到小
        val co = this.area - that.area
        if(co.abs <= 1e-6) 0
        else if(co < 0 ) 1
        else -1
    }*/

    def copyBox() : Box = {
        val Box = new Box(h, w, l, fragility, id)
        Box.pos = pos.copyPosition()
        Box.boxType = boxType
        Box.boxVolume = boxVolume
        Box.area = area
        Box.inoder = inoder
        Box.inoder1 = inoder1
        Box.outorder = outorder
        Box.outorder1 = outorder1
        Box.combineUpId = combineUpId
        Box.combineDownId = combineDownId
        Box.fitness = fitness
        Box.maxW = maxW
        Box
    }

    def resetDirection():Unit = {           //重置箱子方向，boxType設爲0
        if(boxType == 1){
            changeLW()
            boxType = 0
        } else if(boxType ==2){
            changeWH()
            boxType = 0
        }else if(boxType==3){
            changeLH()
            boxType=0
        }
    /*    else if(boxType==4){
            changeHLW()
            boxType=0
        }else if(boxType==5){
            changeWHL()
            boxType=0
        }*/
    }

    def changeLW():Unit = {          //type =1
        val temp = l
        l = w
        w = temp
    }
    def changeWH() : Unit = {         //type =2
        val temp = h
        h = w
        w = temp
    }

    def changeLH():Unit = {            //type = 3
        val temp = l
        l = h
        h = temp
    }
    def changeHLW():Unit = {           //type = 4
        changeLH()
        changeWH()
    }
    def changeWHL():Unit = {           //type = 5
        changeLW()
        changeWH()
    }

    override def toString(): String = {
        "BoxId :" + id + "length :" + l + "width:" + w + "height:" + h + "Isfragility:" + fragility + "BoxType:" + boxType
    }

}
