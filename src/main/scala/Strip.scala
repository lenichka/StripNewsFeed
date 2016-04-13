import scala.collection.immutable.SortedMap

/**
  * Created by helen on 4/9/2016.
  */
class Strip[T] ( val depth: Int, val hashKey:Any => Long){

  var currentMarker:Long = 0
  var store = SortedMap.empty[Long,Any]
  var storeHashKeys = SortedMap.empty[Long,Long] // (key = hashKey(element), element = store.Key)

  def listToAdd(xs: List[Any]): List[Any] = {
    val listToAdd = xs.filter(x => storeHashKeys.keySet.contains(hashKey(x)) == false)
    println("inSet: " + xs)
    println("setToAdd: " + listToAdd)
    listToAdd
  }

  def add[T](xs: List[T]): Long =  xs match {
    case List() => currentMarker
    case z::zs => {
      val addList = listToAdd(xs)
      val listMap = (currentMarker + 1 to currentMarker + addList.length)zip(addList)
      store = SortedMap( listMap: _*) ++ store
      if (store.size > depth) store = store.drop(store.size-depth)

      storeHashKeys = storeHashKeys ++ store.map{case(k,e) => (hashKey(e),k)}
      currentMarker = currentMarker + addList.length
      println("storeHashKeys: " + storeHashKeys)
      println("store: " + store)
      currentMarker
    }
  }

  def getUp(count:Int, marker:Long): (List[Any],Long) =  {
    var ret = SortedMap.empty[Long,Any]
    if (marker < store.firstKey) {
      ret = store.take(count)
    }
    if (marker > store.lastKey) {
      ret = store.from(store.lastKey - count + 1)
    }
    else {
      ret = store.from(marker).take(count)
    }
    println("ret: " + ret)
    println("list: " + ret.toList.unzip._2)
    println("new marker: " + ret.lastKey)
    (ret.toList.unzip._2, ret.lastKey)
  }

  def getDown(count:Int, marker:Long): (List[Any],Long) =  {
    var ret = SortedMap.empty[Long,Any]
    if (marker < store.firstKey) {
      ret = store.take(count)
    }
    if (marker > store.lastKey) {
      ret = store.from(store.lastKey - count + 1)
    }
    else {
      // may run over marker, force a new marker?!
      ret = store.from(marker-count).take(count)
    }
    println("ret: " + ret)
    println("list: " + ret.toList.unzip._2)
    println("new marker: " + ret.lastKey)
    (ret.toList.unzip._2, ret.lastKey)
  }

}

