package random.simulator

import java.security.SecureRandom

import scala.util.Random

object Sim extends App {

  val RunNum = 10000000
  val MaxPow = 10
  val TicketsPerBlock = 10
  val MaxM = 255

  // balance distribution
  val accs = (1 to 1000).map(x => Random.nextInt(10000))
  val fullBalance = accs.sum
  require(accs.forall(b => b.toDouble / fullBalance * TicketsPerBlock < 1))

  var vins = accs.map(x => 0)
  var totalTickets = 0
  for (run <- 1 to RunNum) {
    val scores: Seq[BigInt] = accs.map { b =>
      val m = Random.nextInt(MaxM)
      if (m.toDouble / MaxM < b.toDouble / fullBalance * TicketsPerBlock) {
        score(b, BigInt(m))
      } else {
        BigInt(0)
      }
    }
    val tickets = scores.count(s => s > 0)
    totalTickets += tickets
    val winner = scores.zipWithIndex.maxBy(_._1)._2
    vins = vins.patch(winner, Seq(vins(winner) + 1), 1)
  }
  println("Tickets per block: " + totalTickets.toDouble / RunNum)
  (0 until 1000) foreach { i =>
    val v = vins(i)
    lazy val a = accs(i)
    if (v > 0) {
      println(s"(${v.toDouble / RunNum}, ${a.toDouble / fullBalance}")
    }
  }

  def score(balance: Long, m: BigInt): BigInt = {
    m + 1
  }.ensuring(_ > 0)

  def randomBytes(howMany: Int) = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides r
    r
  }
}
