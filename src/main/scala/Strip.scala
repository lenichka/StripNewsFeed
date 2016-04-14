import scala.collection.immutable.SortedMap

/**
  * Created by helen on 4/9/2016.
  */
class Strip[T] ( val depth: Int, val hashFunction: T => Long){

  var currentMarker:Long = 0
  var store = SortedMap.empty[Long, T]
  var hashKeys = SortedMap.empty[Long, Long] // (key = hashKey(element), element = store.Key)

  def filter(xs: List[T]): List[T] = xs.filterNot(x => hashKeys.contains(hashFunction(x)))

  def add(xs: List[T]): Long =  xs match {
    case List() => currentMarker
    case z::zs =>
      val addList = filter(xs)
      val listMap = (currentMarker + 1 to currentMarker + addList.length)zip(addList)
      store = SortedMap( listMap: _*) ++ store
      if (store.size > depth) store = store.drop(store.size-depth)

      hashKeys = hashKeys ++ store.map{case(k,e) => (hashFunction(e),k)}
      currentMarker = currentMarker + addList.length
//      println("storeHashKeys: " + storeHashKeys)
//      println("store: " + store)
      currentMarker
  }

  def getUp(count:Int, marker:Long): (List[T],Long) =  {
    var ret = SortedMap.empty[Long, T]
    if (marker < store.firstKey) {
      ret = store.take(count)
    }
    if (marker > store.lastKey) {
      ret = store.from(store.lastKey - count + 1)
    }
    else {
      ret = store.from(marker).take(count)
    }
//    println("ret: " + ret)
//    println("list: " + ret.toList.unzip._2)
//    println("new marker: " + ret.lastKey)
    (ret.toList.unzip._2, ret.lastKey)
  }

  def getDown(count:Int, marker:Long): (List[T], Long) =  {
    var ret = SortedMap.empty[Long, T]
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
//    println("ret: " + ret)
//    println("list: " + ret.toList.unzip._2)
//    println("new marker: " + ret.lastKey)
    (ret.toList.unzip._2, ret.lastKey)
  }

}

