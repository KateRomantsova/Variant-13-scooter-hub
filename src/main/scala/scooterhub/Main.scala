package scooterhub

import scala.io.Source
import scala.util.Using

@main def run(path: String): Unit =
  val lines = Using.resource(Source.fromFile(path, "UTF-8"))(_.getLines().drop(1).toList) // drop(1): заголовок
  val (parseErrs, events) = Parser.parseBatch(lines)
  val (fleet, bizErrs)    = Rules.replay(events)
  def r2(x: BigDecimal) = x.setScale(2, BigDecimal.RoundingMode.HALF_UP)
  println(s"Рядків: ${lines.size}, подій: ${events.size}, помилок парсингу: ${parseErrs.size}, бізнес-помилок: ${bizErrs.size}")
  println(s"У хабі: ${Rules.hubCount(fleet)}, готових до поїздки (>20%): ${Rules.readyCount(fleet)}")
  println(s"Середній заряд: ${Rules.avgCharge(fleet).map(r2)}%")
  println(s"Частка, що потребує зарядки/сервісу: ${Rules.shareNeedingService(fleet).map(s => r2(s * 100))}%")
  println(s"Порушення інваріантів: ${Rules.invariants(fleet)}")
  parseErrs.take(5).foreach(println)
  bizErrs.take(5).foreach(println)
