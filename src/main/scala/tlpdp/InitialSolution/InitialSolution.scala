package tlpdp.InitialSolution

import tlpdp.common.Common
import tlpdp.loadFeasible.{BasicHeuristic, MultiLayerSearch, Packing}
import tlpdp.model._
import tlpdp.pdp.Datas

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map


/**
  * Created by hc on 17-1-3.
  */
class InitialSolution(val data : Datas) extends Serializable{

  val vehicleMaxNum : Int = data.vehicleMaxNum               //需要數據傳輸
  val requests : Array[Request] = data.requests.clone()           //從下標0開始
  val customers : Array[Customer] = data.customers.clone()
  val numCUs = customers.length
  var vehicles : Array[Vehicle] = new Array[Vehicle](vehicleMaxNum)
  vehicles = vehicles.map ( v => data.vehicle.copyVehicle())

  val M : Double = 1000.0

  var loadPattern:Map[String,Boolean] = Map()
  var patternIte : Int = 0;

  for(i <- 0 until vehicles.length) vehicles(i).id = i

  /*---------------------calculate cost---------------------------------*/
  val costs : Array[Array[Double]] =(         //distance array
    for (i <- 0 until numCUs) yield{
      (
        for(j <- 0 until numCUs) yield {
          twoPointCost(customers(i),customers(j))
        }).toArray
    }).toArray

  /**
    * distance between customer
    */
  def twoPointCost(c1 : Customer , c2 : Customer) : Double ={
    if(c1.equals(c2)) 0.0
    else{
      Math.sqrt(Math.pow(c1.x - c2.x, 2) + Math.pow(c1.y - c2.y, 2))
    }
  }

  /**
    * dis 直接使用costs數組
    */
  def dis(i : Int , j : Int) : Double = {
    costs(i)(j)
  }
  /*------------------calculate cost end---------------------------------*/
  /*------------------calculate relateness---------------------------------*/
  /**
    * request相似度
    */
  val relatedness : Array[Array[Double]] = (
    for(i <- 0 until requests.length) yield{
      (
        for(j <- 0 until requests.length) yield {
          twoRequestRelated(requests(i), requests(j))
        }).toArray
    }).toArray

  def twoRequestRelated(r1 : Request, r2 : Request) : Double = {
    val a = 8
    val b = 2
    if(r1.equals(r2)) 0.0
    else{
      a * dis(r1.pick.id, r2.pick.id) + dis(r1.delivery.id, r2.delivery.id) + b * (r1.boxesWeight - r2.boxesWeight).abs
    }
  }

  /**
    * rel 直接使用relatedness數組查詢
    */
  def rel(i : Int , j : Int) : Double = {
    relatedness(i)(j)
  }
  /*--------------------calculate relateness end--------------------------*/

  val useTime = false             //控制是否啓用時間窗約束
  val useLoading =true           //控制是否啓用loading
  /**
    * 將request 插入到路徑的第i和第j個位置，得到增加的費用,不包含時間窗 i < j, 倉庫算第0個位置,vehicle.customer[0]乘客總數（序列不包含倉庫)
    * @param vehicle
    * @param request
    * @param i
    * @param j
    * @return
    */
  def addCost (vehicle : Vehicle, request: Request, i : Int, j : Int, cost:Double):Double = {
    if(!feasibleCapacity(vehicle,request,i,j)){         //單純1D載重約束，還未考慮3D RS等約束
      Double.PositiveInfinity
    }else if(useTime && !feasibleTimeWindow(vehicle,request,i,j)){         //校驗時間窗
      Double.PositiveInfinity
    }else if(useLoading && !feasibleLoading(vehicle,request,i,j)){               //3D約束
      Double.PositiveInfinity
    } else{
      cost
    }

  }

  /**
    * 不包括裝載約束
    * @param vehicle
    * @param request
    * @param i
    * @param j
    * @param cost
    * @return
    */
  def addCostWithoutLoad (vehicle : Vehicle, request: Request, i : Int, j : Int, cost:Double):Double = {
    if(!feasibleCapacity(vehicle,request,i,j)){         //單純1D載重約束，還未考慮3D RS等約束
      Double.PositiveInfinity
    }else if(useTime && !feasibleTimeWindow(vehicle,request,i,j)){         //校驗時間窗
      Double.PositiveInfinity
    }else{
      cost
    }
  }

  /**
    * 單純計算cost
    * @param vehicle
    * @param request
    * @param i
    * @param j
    * @return
    */
  def addCostNotConstraint(vehicle : Vehicle, request: Request, i : Int, j : Int): Double = {
    val pid = request.pick.id
    val did = request.delivery.id
    var c : Double =0.0
    val pl = if (i==1) 0 else vehicle.customers(i-1)
    val pr = if(i == j-1) did else vehicle.customers(i)
    val dl = if(i == j-1) pid else vehicle.customers(j-2)
    val dr = if (j == vehicle.customers(0)+2) 0 else vehicle.customers(j-1)

    if(i == j-1){
      c = c + dis(pl, pid) + dis(pr,dl) + dis(did,dr) - dis(pl, dr)
    }else{
      c = c + dis(pl, pid) + dis(pid, pr) + dis(dl, did) + dis(did, dr) - dis(pl,pr) - dis(dl,dr)
    }
    c
  }
  /**
    * 判斷在添加request後，受影響的一段路徑內載重量是否超出
    * @param vehicle
    * @param request
    * @param i
    * @param j
    * @return
    */
  def feasibleCapacity (vehicle: Vehicle, request: Request,i : Int, j: Int) : Boolean = {
    var flag : Boolean = true
    if(i == j-1){
      if(Common.less(vehicle.capacity, vehicle.nowCapacity(i-1) + request.boxesWeight)) flag = false
    }else{
      for(ite <- i-1 to j-2) {
        if(Common.less(vehicle.capacity, vehicle.nowCapacity(ite) + request.boxesWeight)) flag = false
      }
    }
    flag
  }

  def feasibleTimeWindow(vehicle: Vehicle, request: Request, i : Int, j : Int) : Boolean = {
    var t: Double = 0
    var customerOrder : ArrayBuffer[Int] = new ArrayBuffer[Int]()
    customerOrder ++= vehicle.customers
    customerOrder.insert(i,request.pick.id)
    customerOrder.insert(j,request.delivery.id)
    customerOrder(0) = 0         //設爲倉庫節點
    for(c <- 1 until customerOrder.length){
      t += costs(customerOrder(c-1))(customerOrder(c))
      if(Common.more(t , customers(customerOrder(c)).latestTime))  return false
      if(Common.less(t , customers(customerOrder(c)).earlyTime))
        t = customers(customerOrder(c)).earlyTime + customers(customerOrder(c)).ServiceTime
      else
        t += customers(customerOrder(c)).ServiceTime
    }
    t += costs(customerOrder(customerOrder.length -1))(0)
    if(Common.more(t, customers(0).latestTime)) return false
    true
  }

  /**
    * 三維裝載可行性
    * @param vehicle
    * @param request
    * @param i
    * @param j
    * @return
    */
  def feasibleLoading(vehicle: Vehicle, request: Request, i: Int, j: Int) : Boolean = {
//    return true         //1l-pdp
    var origin : Position = new Position()    //創建原點坐標
    origin.lx = vehicle.length
    origin.ly = vehicle.width
    origin.lz = vehicle.height
    var boxes : ArrayBuffer[Box] = new ArrayBuffer[Box]()           //total boxes
    val st = getHashOrder(request)       //將驗證過的裝箱存入hash表中
/*    if(loadPattern.contains(st)&& i+1==j){
      return true
    }
*/
    for(ite <- i-1 to j-2)
    {
      if(true || !isPack(vehicle,ite, i,j))
        {
          boxes.clear()
          for (r <- 1 to ite) {
            if (this.customers(vehicle.customers(r)).character == 0) {
              val outOrder = getOutOrder(vehicle.customers, r, i, j)
              if (r < i)
                boxes ++= getCustomerBox(getRequestId(vehicle.customers(r)), r, outOrder) //獲取pick的箱子並且賦予箱子pick與delivery的順序
              else
                boxes ++= getCustomerBox(getRequestId(vehicle.customers(r)), r + 1, outOrder) //獲取pick的箱子並且賦予箱子pick與delivery的順序
            } else {
              val p: Int = if (r < i) r else if (r <= j - 2) r + 1 else r + 2
              removeBox(boxes, p)
            }
          }
          if(!boxes.isEmpty || (boxes.isEmpty && !loadPattern.contains(st))){
            boxes ++= getCustomerBox(request, i, j)
            var tolVolume : Double = 0.0
            for(b <- boxes) tolVolume += b.boxVolume
            if(Common.more(tolVolume, origin.lx*origin.ly*origin.lz)) return false
            val ps: Array[Int] = new Array[Int](boxes.length)
            //          val bh = new BasicHeuristic(ps, origin, boxes)
            val bh = new MultiLayerSearch(origin,boxes)
            //          val bh = new Packing(origin,boxes)
            //          if (!bh.packing()) {
            //          println("boxes"+ boxes.toString())

            if(!bh.basicHeuristic()){
              return false
            }
            if(boxes.isEmpty && !loadPattern.contains(st))
              loadPattern += (st -> true)
          }
        }
    }
/*    patternIte += 1
    if(patternIte >= 5000){
      loadPattern.clear()
      patternIte = 0
    }*/
//    loadPattern += (st -> true)
    true
  }

  def isPack(vehicle: Vehicle,ite : Int,i: Int, j: Int): Boolean = {
    if(ite+1<=vehicle.customers(0) && i+1!=j && this.customers(vehicle.customers(ite+1)).character==0 && ite+1 < j-1)
      return true
    if(this.customers(vehicle.customers(ite)).character != 0 && i+1!=j && ite!=0 && ite+1 < i)
      return true
    false
  }
  /**
    * 獲取request id
    * @param cid
    * @return
    */
  def getRequestId(cid : Int) : Request = {
    val rid = this.customers(cid).requestId
    requests(rid-1)              //requestid爲1的數組下標爲0
  }

  /**
    * 檢查單個request是否符合裝箱約束
    */
  def getHashOrder(request: Request):String = {
    var s:String = ""
/*    for(cus <- 1 to i-1) s= s+ vehicle.customers(cus) + "*"
    s += request.pick.id + "*"
    for(cus <- i to j-2) s= s+ vehicle.customers(cus) + "*"
    s += request.delivery.id + "*"
    s*/
    s += request.id
    s
  }
  /**
    * 獲取相應request的boxes
    * @param request
    * @param r
    * @return
    */
  def getCustomerBox(request: Request, r : Int, out : Int) : ArrayBuffer[Box] = {
    var boxes : ArrayBuffer[Box] = new ArrayBuffer[Box]()
    boxes ++= request.boxes
    for(b <- boxes){
      b.inoder = r
      b.outorder = out
    }
    boxes
  }

  /**
    * 獲取delivery的位置，傳入pick信息
    * @param cus
    * @param r
    * @param i
    * @param j
    * @return
    */
  def getOutOrder(cus : ArrayBuffer[Int], r : Int, i: Int, j: Int) : Int = {
    val request = getRequestId(cus(r))
    val did = request.delivery.id
    var p : Int = 0
    for(ite <- 1 until cus.length if did == cus(ite)){
      p = ite
    }
    if(p < i) p
    else if(p <= j-2) p+1
    else p+2
  }

  def removeBox(boxes : ArrayBuffer[Box], oid : Int): Unit = {
    val b = boxes.filter(b => b.outorder != oid)
    boxes.clear()
    boxes ++= b
  }
  /**
    *將request插入到vehicle的第i和j位置
    */
  def addRequest(vehicle: Vehicle, request: Request, i : Int, j : Int): Unit = {
      addCustomer(vehicle, request.pick.id, i)
      addCustomer(vehicle, request.delivery.id, j)
      vehicle.requests += request
      addCapacity(vehicle, request.boxesWeight, i : Int, j : Int)   //修改capacity數組
  }

  /**
    * 將單個乘客i插入到vehicle的第j個位置
    */
  def addCustomer (vehicle: Vehicle , i : Int, j : Int) : Unit = {
      vehicle.customers.insert(j,i)
      vehicle.nowCapacity.insert(j,0)
      vehicle.customers(0) += 1
  }

  /**
    * 修改capacity數組，將 i 到 j 位置之間的車輛載重都填上當前增加的request重量
    */
  def addCapacity(vehicle: Vehicle, weight : Double , i : Int , j : Int ) : Unit = {
    for (ite <- i+1 to j-1) vehicle.nowCapacity(ite) += weight
    vehicle.nowCapacity(i) = vehicle.nowCapacity(i-1) + weight
    vehicle.nowCapacity(j) = vehicle.nowCapacity(j-1) - weight
  }

  /**
    * 計算總的代價（包括時間窗）
    */
  def calCost(veh: Array[Vehicle]) : Double = {
    var cost : Double = 0.0
    var costOneV : Double = 0.0
    for(i <- 0 until veh.length){
      if(veh(i).customers(0) != 0){
        costOneV = 0.0
        for(j <- 1 to veh(i).customers(0)){
          if(j == 1) {
            costOneV += costs(0)(veh(i).customers(j))
            if(Common.less(costOneV,customers(veh(i).customers(j)).earlyTime))  costOneV = customers(veh(i).customers(j)).earlyTime + customers(veh(i).customers(j)).ServiceTime
            else costOneV += customers(veh(i).customers(j)).ServiceTime
          }else{
            costOneV += costs(veh(i).customers(j-1))(veh(i).customers(j))
            if(Common.less(costOneV,customers(veh(i).customers(j)).earlyTime))  costOneV = customers(veh(i).customers(j)).earlyTime + customers(veh(i).customers(j)).ServiceTime
            else costOneV += customers(veh(i).customers(j)).ServiceTime
          }
        }
        costOneV += costs(veh(i).customers(veh(i).customers(0)))(0)
        veh(i).cost = costOneV
      }
      cost += costOneV
    }
    cost
  }

  /**
    *計算代價不包括時間窗，只有distance
    */
  def calDistance(veh : Array[Vehicle]) : Double = {
    var distance : Double = 0.0
    for(i <- 0 until veh.length){
      distance += veh(i).distance
    }
    distance
  }

  /**
    *計算代價包括missrequest
    */
  def calCostAddMissRequest(M : Double, cost : Double, missnum : Int) : Double = {
    val totalCost : Double = cost + M * missnum
    totalCost
  }

  /**
    * 獲取有載客的車的數量
    */
  def calCarNum(sol : Solution):Int = {
    var carnum : Int = 0
    for(i <- 0 until sol.vehicles.length if sol.vehicles(i).customers(0)!=0){
      carnum += 1
    }
    carnum
  }

  //用於存放request插入一條路徑後的最佳位置 id - vehicle id ; cost 增加的費用
  class insertPair(val id : Int, val pi : Int, val di : Int, val cost : Double) extends Ordered[insertPair]{
    def compare(that : insertPair) : Int = {
      //按cost 順序
      if(this.cost == Double.PositiveInfinity || that.cost==Double.PositiveInfinity){
        if(this.cost == Double.PositiveInfinity && that.cost==Double.PositiveInfinity) {
          if((this.id - that.id) > 0) 1 else -1
        }
        else if(this.cost == Double.PositiveInfinity ) return 1
        else return -1
      }
      val co = this.cost - that.cost
      if(co.abs <= 1e-6) {
        if((this.id - that.id) > 0) 1 else -1
      }
      else if(co > 0) 1
      else{
        if((this.id - that.id) > 0) 1 else -1
      }        //Double.Infinity相減爲NAN
    }
  }


}
