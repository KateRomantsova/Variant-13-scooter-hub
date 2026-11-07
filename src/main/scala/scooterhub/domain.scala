package scooterhub

enum ParseError:
  case MalformedRow(row: Int, raw: String, expectedCols: Int, actualCols: Int)
  case UnknownEvent(row: Int, code: String)
  case InvalidToken(row: Int, raw: String)
  case InvalidQuantity(row: Int, raw: String, reason: String)
  case InvalidPercent(row: Int, raw: String, reason: String)

// ----- Атоми (Smart Constructors) -----
final case class ScooterId private (value: String)
object ScooterId:
  private val Pattern = "[A-Za-z0-9._-]{1,64}".r
  def fromString(row: Int, raw: String): Either[ParseError, ScooterId] =
    raw match
      case Pattern() => Right(new ScooterId(raw))
      case _         => Left(ParseError.InvalidToken(row, raw))

final case class Charge private (value: BigDecimal)
object Charge:
  private val Pattern = """-?\d+(?:\.\d{1,2})?""".r // десяткове, до 2 знаків
  def fromString(row: Int, raw: String): Either[ParseError, Charge] =
    raw match
      case Pattern() =>
        val v = BigDecimal(raw) // формат уже перевірено, не кине
        if v < 0 || v > 100 then Left(ParseError.InvalidPercent(row, raw, "поза діапазоном 0..100"))
        else Right(new Charge(v))
      case _ => Left(ParseError.InvalidPercent(row, raw, "не десяткове число (до 2 знаків)"))

final case class Count private (value: Int)
object Count:
  private val Pattern = """-?\d+""".r
  def fromString(row: Int, raw: String): Either[ParseError, Count] =
    raw match
      case Pattern() =>
        raw.toIntOption match
          case Some(v) if v > 0 => Right(new Count(v))
          case Some(_)          => Left(ParseError.InvalidQuantity(row, raw, "має бути > 0"))
          case None             => Left(ParseError.InvalidQuantity(row, raw, "завелике число"))
      case _ => Left(ParseError.InvalidQuantity(row, raw, "не ціле число"))

// ----- Доменні події -----
enum Event:
  case Restock(count: Count)
  case RentOut(id: ScooterId)
  case Returned(id: ScooterId, charge: Charge)
  case RemoveForService(id: ScooterId)

// ----- Бізнес-помилки (етап згортання) -----
enum BusinessError:
  case AlreadyRented(id: ScooterId)
  case NotRented(id: ScooterId)
  case ChargeTooLow(id: ScooterId, charge: BigDecimal)
  case ScooterInService(id: ScooterId)
  case RentedCannotBeServiced(id: ScooterId)
  case NoScooterInHub(id: ScooterId)

// ----- Стан -----
enum Status { case InHub, Rented, InService }
final case class Scooter(charge: BigDecimal, status: Status)
/** fresh: доставлені самокати 100% без відстеженого id; scooters: ті, що вже мають id. */
final case class Fleet(fresh: Int = 0, scooters: Map[ScooterId, Scooter] = Map.empty)
