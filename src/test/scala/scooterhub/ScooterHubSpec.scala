package scooterhub

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

class ScooterHubSpec extends AnyFlatSpec with Matchers:

  private def run(lines: String*) =
    val (pe, evs) = Parser.parseBatch(lines.toList)
    val (fleet, be) = Rules.replay(evs)
    (pe, fleet, be)

  "replay" should "fold a short scenario into state and metrics" in {
    val (pe, fleet, be) = run("RESTOCK,5,,,", "RENT_OUT,s1,,,", "RETURN,s1,80.00,,")
    pe shouldBe empty
    be shouldBe empty
    Rules.hubCount(fleet) shouldBe 5
    Rules.readyCount(fleet) shouldBe 5
    Rules.avgCharge(fleet) shouldBe Some(BigDecimal(96)) // (4*100 + 80) / 5
    Rules.invariants(fleet) shouldBe empty
  }

  it should "compute ready count, average and share needing charge or service" in {
    val (_, fleet, be) = run("RESTOCK,3,,,", "RENT_OUT,s1,,,", "RETURN,s1,18.00,,", "REMOVE_FOR_SERVICE,s2,,,")
    be shouldBe empty
    Rules.hubCount(fleet) shouldBe 2            // 1 свіжий + s1
    Rules.readyCount(fleet) shouldBe 1          // s1 (18%) не готовий
    Rules.avgCharge(fleet) shouldBe Some(BigDecimal(59))
    Rules.shareNeedingService(fleet).map(_.setScale(4, BigDecimal.RoundingMode.HALF_UP)) shouldBe Some(BigDecimal("0.6667")) // (s1 + s2) / 3
  }

  "parser" should "send invalid rows to the Left rail" in {
    Parser.parseRow(1, "NOT_AN_EVENT,1,,,") should matchPattern { case Left(ParseError.UnknownEvent(1, "NOT_AN_EVENT")) => }
    Parser.parseRow(2, "RESTOCK,3") should matchPattern { case Left(ParseError.MalformedRow(2, _, 5, 2)) => }
    Parser.parseRow(3, "RESTOCK,3,,,,") should matchPattern { case Left(ParseError.MalformedRow(3, _, 5, 6)) => }
    Parser.parseRow(4, ",s1,,,") should matchPattern { case Left(ParseError.MalformedRow(4, _, 5, 5)) => }
    Parser.parseRow(5, "RETURN,s1,abc,,") should matchPattern { case Left(ParseError.InvalidPercent(5, "abc", _)) => }
    Parser.parseRow(6, "RETURN,s1,150.0,,") should matchPattern { case Left(ParseError.InvalidPercent(6, _, _)) => }
    Parser.parseRow(7, "RETURN,s1,-1,,") should matchPattern { case Left(ParseError.InvalidPercent(7, _, _)) => }
    Parser.parseRow(8, "RESTOCK,0,,,") should matchPattern { case Left(ParseError.InvalidQuantity(8, "0", _)) => }
    Parser.parseRow(9, "RESTOCK,3.5,,,") should matchPattern { case Left(ParseError.InvalidQuantity(9, _, _)) => }
    Parser.parseRow(10, "RENT_OUT,bad token,,,") should matchPattern { case Left(ParseError.InvalidToken(10, _)) => }
  }

  it should "keep trailing empty fields (split with -1)" in {
    Parser.parseRow(1, "RENT_OUT,s1,,,").isRight shouldBe true
  }

  "invalid row" should "not change the state" in {
    val (pe, fleet, _) = run("RESTOCK,5,,,", "RESTOCK,xx,,,")
    pe.size shouldBe 1
    fleet.fresh shouldBe 5
  }

  "invariant forbid_rent_if_charge_below_15" should "reject rent at 14.99% and allow at 15.00%" in {
    val (_, _, low) = run("RESTOCK,3,,,", "RENT_OUT,s1,,,", "RETURN,s1,14.99,,", "RENT_OUT,s1,,,")
    low.map(_._2) should matchPattern { case List(BusinessError.ChargeTooLow(_, _)) => }

    val (_, fleet, ok) = run("RESTOCK,3,,,", "RENT_OUT,s1,,,", "RETURN,s1,15.00,,", "RENT_OUT,s1,,,")
    ok shouldBe empty
    fleet.scooters.values.map(_.status) should contain only Status.Rented
  }

  "business rules" should "reject duplicate rent, dangling return and service of rented scooter" in {
    val (_, _, be) = run("RESTOCK,5,,,", "RENT_OUT,s1,,,", "RENT_OUT,s1,,,", "RETURN,s2,50.00,,", "REMOVE_FOR_SERVICE,s1,,,")
    be.map(_._2) should matchPattern {
      case List(BusinessError.AlreadyRented(_), BusinessError.NotRented(_), BusinessError.RentedCannotBeServiced(_)) =>
    }
  }

  it should "reject rent when the hub is empty" in {
    val (_, _, be) = run("RENT_OUT,s1,,,")
    be.map(_._2) should matchPattern { case List(BusinessError.NoScooterInHub(_)) => }
  }

  "importer" should "survive a large log with ~5% dirty rows" in {
    val good = List.fill(950)("RESTOCK,3,,,")
    val bad  = List("NOT_AN_EVENT,,,,", "RESTOCK,abc,,,", "RETURN,s1", "", "RETURN,s1,150.00,,")
    val lines = Random(42).shuffle(good ++ bad.flatMap(List.fill(10)(_)))
    val (pe, evs) = Parser.parseBatch(lines)
    pe.size shouldBe 50
    evs.size shouldBe 950
    Rules.replay(evs)._1.fresh shouldBe 2850
  }
