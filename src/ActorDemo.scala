import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem
import akka.routing.RoundRobinPool

//定义一个封闭的特质
sealed trait SumTrait

// 定义一个case类，混合特质SumTrait
case class Result(value: Int) extends SumTrait

//sealed case class Result(value:Int)

// 计算用的Actor
class SumActor extends Actor {
  val RANGE = 10000

  def calculate(start: Int, end: Int, flag : String): Int = {
    var cal = 0

    for (i <- (start to end)) {
      for (j <- 1 to 3000000) {}
      cal += i
    }

    println("flag : " + flag + ".")
    return cal
  }

  def receive = {
    //返回计算结果
    case value: Int => sender ! Result( calculate((RANGE / 4) * (value - 1) + 1, (RANGE / 4) * value, value.toString) )

    case _ => println("未知 in SumActor...")
  }
}

// 打印结果用的Actor
class PrintActor extends Actor {
  def receive = {
    case (sum: Int, startTime: Long) => println("总数为：" + sum + "；所花时间为：" + (System.nanoTime() - startTime)/1000000000.0 + "秒。")
    case _ => println("未知 in PrintActor...")
  }
}

// 主actor，发送计算指令给SumActor，发送打印指令给PrintActor
class MasterActor extends Actor {
  var sum = 0
  var count = 0
  var startTime: Long = 0

  // 声明Actor实例，nrOfInstances是pool里所启routee（SumActor）的数量，
  // 这里用4个SumActor来同时计算，很Powerful。
  val sumActor   = context.actorOf( Props[SumActor].withRouter( RoundRobinPool(nrOfInstances = 4) ), name = "sumActor" )

  val printActor = context.actorOf(Props[PrintActor], name = "printActor")

  def receive = {
    //常量模式匹配
    case "calculate..." =>
      startTime = System.nanoTime()
      for (i <- 1 to 4) sumActor ! i

    //构造器模式匹配
    case Result(value) =>
      sum += value
      count += 1
      if (count == 4) {
        printActor ! (sum, startTime)
        context.stop(self)
      }

    case _ => println("未知 in MasterActor...")
  }
}

object Sum {
  def main(args: Array[String]): Unit = {
    var sum = 0

    val system = ActorSystem("MasterActorSystem")
    val masterActor = system.actorOf(Props[MasterActor], name = "masterActor")
    masterActor ! "calculate..."

    //Thread.sleep(5000)
  }
}