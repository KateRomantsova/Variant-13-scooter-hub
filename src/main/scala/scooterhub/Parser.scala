package scooterhub

object Parser:
  private val Cols = 5

  def parseRow(row: Int, line: String): Either[ParseError, Event] =
    line.split(",", -1) match // -1: не губимо хвостові порожні поля
      case Array(code, p1, p2, _, _) if code.trim.nonEmpty =>
        code.trim match
          case "RESTOCK" =>
            Count.fromString(row, p1).map(Event.Restock(_))
          case "RENT_OUT" =>
            ScooterId.fromString(row, p1).map(Event.RentOut(_))
          case "RETURN" =>
            for
              id <- ScooterId.fromString(row, p1)
              c  <- Charge.fromString(row, p2)
            yield Event.Returned(id, c)
          case "REMOVE_FOR_SERVICE" =>
            ScooterId.fromString(row, p1).map(Event.RemoveForService(_))
          case other => Left(ParseError.UnknownEvent(row, other))
      case arr => // не 5 колонок (truncate / extra_column) або порожній event
        Left(ParseError.MalformedRow(row, line, Cols, arr.length))

  def parseBatch(lines: List[String]): (List[ParseError], List[(Int, Event)]) =
    lines.zipWithIndex.partitionMap { case (line, idx) =>
      val row = idx + 1
      parseRow(row, line).map(ev => (row, ev))
    }
