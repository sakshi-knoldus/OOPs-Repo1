trait Queue{
  var queue:List[Int]=List.empty
  var front:Int = -1
  var rear:Int = -1
  def enqueue(el: Int):String = {
    if(rear == -1 && front == -1){
      front= front+1
      rear= rear+1
      queue = queue ::: List(el)
      "%d Item is enqueued in Queue."format(el)
    }
    else{
      rear= rear+1
      queue =queue:::List(el)
      "%d Item is enqueued in Queue.".format(el)
    }
  }
  def dequeue: String ={
    if(front == -1 && rear == -1){
      "Queue Underflow.."
    }
    else if(front == rear ){
      queue = queue.drop(1)
      front = -1
      rear = -1
      "Item dequeued.."
    }
    else{
      queue = queue.drop(1)
      front = front + 1
      "Item dequeued.."
    }
  }
  def getQueue:List[Int] ={
    queue
  }
}
class DoubleQueue extends Queue{
  override def enqueue(el: Int): String = super.enqueue(2*el)
}
class SquareQueue extends Queue{
  override def enqueue(el: Int): String = super.enqueue(el*el)
}
object QueueObject{
  val li1= new DoubleQueue
  val li2 = new SquareQueue
  println(li1.enqueue(3))
  println(li1.enqueue(4))
  println(li1.enqueue(6))
  println(li1.getQueue)
  println(li2.enqueue(5))
  println(li2.enqueue(8))
  println(li2.enqueue(2))
  println(li2.dequeue)
  println(li2.getQueue)
}