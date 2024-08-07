package net.degoes

import net.degoes.education_executable.Quiz2
import net.degoes.email_filter2.EmailFilter.subjectContains
import net.degoes.ecommerce_marketing.abstract_encoding.HistoryPattern

/*
 * INTRODUCTION
 *
 * In Functional Design, there are two ways to represent models:
 *
 * 1. Using a function or interface, whose methods execute the solution. This is
 *    called the "executable" encoding in this course. It's a direct, executable
 *    encoding of a domain. If some functional domain is modeled with a class
 *    or case class, or an open trait that is implemented by classes, then it's
 *    probably an executable encoding.
 *
 * 2. Using a pure data structure, which declaratively describes the solution, but
 *    which does not perform the solution. It's an abstract, "declarative"
 *    encoding of a domain. If some functional domain type is modeled with a
 *    enum, then it's probably a declarative encoding, where the subtypes
 *    of the enum model individual operations and constructors in the
 *    domain.
 *
 * In the second encoding, a so-called "executor" or "interpreter" or "compiler"
 * translates the data structure, which merely models a solution, into either
 * executable code or into another lower-level domain, which provides the
 * capabilities modeled by the functional domain.
 *
 * Executable encodings are "open" for new constructors and operators: anyone
 * can add new constructors and operators, without updating existing code. On
 * the other hand, executable encodings are not "introspectable": because they
 * are not data, but rather, opaque executable machinery, it is not possible to
 * add new ways to execute the models without rewriting all constructors and
 * operators.
 *
 * Declarative encodings are "closed" for new constructors and operators: no
 * one can add new constructors and operators, without updating existing code.
 * Yet, because they are pure data, it is easy to add new ways to execute the
 * models, for example, serializers, optimizers, converters, and so forth,
 * assuming their component parts have the same properties (not all
 * declarative encodings do; if you embed a function inside a declarative
 * encoding, it becomes opaque).
 *
 * Summarizing the difference between executable and declarative encodings:
 *
 *  - Executable encodings have unbounded constructors/operators, but a fixed
 *    number of ways to execute them.
 *  - Declarative encodings have fixed constructors/operators, but an unbounded
 *    number of ways to execute them.
 *
 * Note: Tagless-final is an executable encoding, but one where, by making the
 * "solutions" polymorphic, the choice of executor can be deferred arbitrarily.
 *
 * Legacy code prefers executable encodings; while many benefits of Functional
 * Design can be seen best using abstract encodings.
 *
 */

/** EDUCATION - EXERCISE SET 1
  *
  * Consider a console-based educational application that tests the user's knowledge of key
  * concepts.
  */
object education_executable:
  import education.*

  enum Quiz2:
    case Value[A](question: Question[A])
    case Plus(q1: Quiz2, q2: Quiz2)
    case Bonus(q: Quiz2)

    def self = this

    /** EXERCISE 1
      *
      * Add an operator `+` that appends this quiz to the specified quiz. Model this as pure data
      * using a constructor for Quiz in the companion object.
      */
    def +(that: Quiz2): Quiz2 = Quiz2.Plus(that, self)

    /** EXERCISE 2
      *
      * Add a unary operator `bonus` that marks this quiz as a bonus quiz. Model this as pure data
      * using a constructor for Quiz in the companion object.
      */
    def bonus: Quiz2 = Quiz2.Bonus(self)
  end Quiz2
  object Quiz2:
    def apply[A](question: Question[A]): Quiz2 = ???

  private def grade[A](f: String => A, checker: Checker[A]): QuizResult =
    scala.util.Try {
      val submittedAnswer = f(scala.io.StdIn.readLine())

      checker.isCorrect(submittedAnswer) match
        case Left(string)  => QuizResult(0, 0, checker.points, Vector(string))
        case Right(string) => QuizResult(checker.points, 0, 0, Vector.empty)
    }.getOrElse(
      QuizResult(0, 0, checker.points, Vector("The format of your answer was not recognized"))
    )

  /** EXERCISE 3
    *
    * Implement an interpreter for the `Quiz` model that translates it into the interactive console
    * operations that it describes, returning a QuizResult value.
    */
  def run(quiz: Quiz2): QuizResult = quiz match
    case Quiz2.Value(question) =>
      import Question.*

      println(question.question)

      question match
        case Text(question, checker)                    => grade(identity(_), checker)
        case MultipleChoice(question, choices, checker) =>
          val choicePrintout =
            choices.zipWithIndex.map { case (c, i) => s"${i}. ${c}" }.mkString("\n")

          println("Your options are: \n" + choicePrintout)

          grade(_.toInt, checker)
        case TrueFalse(question, checker)               => grade(_.toLowerCase().startsWith("t"), checker)
    case Quiz2.Bonus(q)        => run(q).toBonus
    case Quiz2.Plus(q1, q2)    => run(q1) + run(q2)

end education_executable

/** DATA TRANSFORM - EXERCISE SET 2
  *
  * Consider an email marketing platform, which allows users to upload contacts.
  */
object contact_processing2:
  import contact_processing.*

  enum SchemaMapping2:
    case Add(scheme1: SchemaMapping2, scheme2: SchemaMapping2)
    case Else(rightScheme: SchemaMapping2, leftScheme: SchemaMapping2)
    case Rename(oldName: String, newName: String)
    case Delete(name: String)
    def self = this

    /** EXERCISE 1
      *
      * Add a `+` operator that models combining two schema mappings into one, applying the effects
      * of both in sequential order.
      */
    def +(that: SchemaMapping2): SchemaMapping2 = Add(that, self)

    /** EXERCISE 2
      *
      * Add an `orElse` operator that models combining two schema mappings into one, applying the
      * effects of the first one, unless it fails, and in that case, applying the effects of the
      * second one.
      */
    def orElse(that: SchemaMapping2): SchemaMapping2 = Else(self, that)
  end SchemaMapping2
  object SchemaMapping2:

    /** EXERCISE 3
      *
      * Add a constructor for `SchemaMapping` models renaming the column name.
      */
    def rename(oldName: String, newName: String): SchemaMapping2 = Rename(oldName, newName)

    /** EXERCISE 4
      *
      * Add a constructor for `SchemaMapping` that models deleting the column of the specified name.
      */
    def delete(name: String): SchemaMapping2 = Delete(name)

  /** EXERCISE 5
    *
    * Implement an interpreter for the `SchemaMapping` model that translates it into into changes on
    * the contact list.
    */
  def run(mapping: SchemaMapping2, contacts: ContactsCSV): MappingResult[ContactsCSV] =
    mapping match
      case SchemaMapping2.Rename(oldName, newName) =>
        MappingResult.Success(contacts.rename(oldName, newName), Nil)
      case SchemaMapping2.Delete(name)             =>
        MappingResult.Success(contacts.delete(name), Nil)
      case SchemaMapping2.Add(scheme1, scheme2)    =>
        run(mapping = scheme1, contacts = contacts).flatMap(result1 =>
          run(mapping = scheme2, contacts = result1)
        )
      case SchemaMapping2.Else(that, other)        =>
        run(mapping = that, contacts = contacts).orElse(run(mapping = other, contacts = contacts))

  /** BONUS EXERCISE
    *
    * Implement an optimizer for the `SchemaMapping` model that pushes deletes to the front of the
    * schema mapping in cases where doing so wouldn't later the result.
    */
  def optimize(schemaMapping: SchemaMapping2): SchemaMapping2 =
    schemaMapping match
      case SchemaMapping2.Add(scheme1, scheme2)         => ???
      case SchemaMapping2.Else(rightScheme, leftScheme) => ???
      case SchemaMapping2.Rename(oldName, newName)      => ???
      case SchemaMapping2.Delete(name)                  => ???

  // val m =
  //   delete("foo") + rename(
  //     "bar",
  //     "foo"
  //   ) // Add(Add(Delete("foo"), Rename("bar", "foo")), Delete("bar"))
end contact_processing2

/** EMAIL CLIENT - EXERCISE SET 3
  *
  * Consider a web email interface, which allows users to filter emails and direct them to specific
  * folders based on custom criteria.
  */
object email_filter2:
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  enum EmailFilter:
    case And(filter1: EmailFilter, filter2: EmailFilter)
    case Or(filter1: EmailFilter, filter2: EmailFilter)
    case Not(filter: EmailFilter)
    case BodyContains(subString: String)
    case SubjectContains(subString: String)
    case RecipientIn(reciepients: Set[Address])
    case SenderIn(senders: Set[Address])

    def self = this

    /** EXERCISE 1
      *
      * Add an "and" operator that models matching an email if both the first and the second email
      * filter match the email.
      */
    def &&(that: EmailFilter): EmailFilter = And(self, that)

    /** EXERCISE 2
      *
      * Add an "or" operator that models matching an email if either the first or the second email
      * filter match the email.
      */
    def ||(that: EmailFilter): EmailFilter = Or(self, that)

    /** EXERCISE 3
      *
      * Add a "negate" operator that models matching an email if this email filter does NOT match an
      * email.
      */
    def negate: EmailFilter = Not(self)
  end EmailFilter
  object EmailFilter:

    /** EXERCISE 4
      *
      * Add a constructor for `EmailFilter` that models looking to see if the subject of an email
      * contains the specified word.
      */
    def subjectContains(string: String): EmailFilter = SubjectContains(string)

    /** EXERCISE 5
      *
      * Add a constructor for `EmailFilter` that models looking to see if the body of an email
      * contains the specified word.
      */
    def bodyContains(string: String): EmailFilter = BodyContains(string)

    /** EXERCISE 6
      *
      * Add a constructor for `EmailFilter` that models looking to see if the sender of an email is
      * in the specified set of senders.
      */
    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    /** EXERCISE 7
      *
      * Add a constructor for `EmailFilter` that models looking to see if the recipient of an email
      * is in the specified set of recipients.
      */
    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)
  end EmailFilter

  /** EXERCISE 8
    *
    * Implement an interpreter for the `EmailFilter` model that translates it into into tests on the
    * specified email.
    */
  def matches(filter: EmailFilter, email: Email): Boolean =
    filter match
      case EmailFilter.And(filter1, filter2)    => matches(filter1, email) && matches(filter2, email)
      case EmailFilter.Or(filter1, filter2)     => matches(filter1, email) || matches(filter2, email)
      case EmailFilter.Not(notFilter)           => !matches(notFilter, email)
      case EmailFilter.SubjectContains(string)  => email.subject.contains(string)
      case EmailFilter.BodyContains(string)     => email.body.contains(string)
      case EmailFilter.SenderIn(senders)        => senders.contains(email.sender)
      case EmailFilter.RecipientIn(reciepients) => reciepients.intersect(email.to.toSet).nonEmpty

  val f = EmailFilter.And(EmailFilter.bodyContains("foo"), EmailFilter.bodyContains("bar"))

  /** EXERCISE 9
    *
    * Implement a function to make an English-readable description of an `EmailFilter`.
    */
  def describe(filter: EmailFilter): Unit = filter match
    case EmailFilter.And(filter1, filter2) =>
      print("(")
      describe(filter1)
      print(" and ")
      describe(filter2)
      print(")")
    // ...
end email_filter2

/** SPREADSHEET - EXERCISE SET 4
  *
  * Consider a spreadsheet application with a bunch of cells, containing either static data or
  * formula computed from other cells.
  */
object spreadsheet2:
  trait Spreadsheet:
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue

    final def scan(range: Range): LazyList[Cell] =
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for
        col <- (minCol to maxCol).to(LazyList)
        row <- (minRow to maxRow).to(LazyList)
      yield Cell(col, row, valueAt(col, row)))

  final case class Range(
    minRow: Option[Int],
    maxRow: Option[Int],
    minCol: Option[Int],
    maxCol: Option[Int]
  )
  object Range:
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)

  final case class Cell(col: Int, row: Int, contents: CalculatedValue)

  enum Value:
    case Error(message: String)
    case Str(value: String)
    case Dbl(value: Double)

  enum CalculatedValue:
    case Sum(value1: CalculatedValue, value2: CalculatedValue)
    case Not(value: CalculatedValue)
    case Const(contents: Value)
    case At(col: Int, row: Int)

    def self = this

    /** EXERCISE 1
      *
      * Add some operators to transform one `CalculatedValue` into another `CalculatedValue`. For
      * example, one operator could "negate" a double CalculatedValue.
      */
    def negate: CalculatedValue = Not(self)

    /** EXERCISE 2
      *
      * Add some operators to combine `CalculatedValue`. For example, one operator could sum two
      * double CalculatedValueessions.
      */
    def sum(that: CalculatedValue): CalculatedValue = Sum(self, that)
  end CalculatedValue
  object CalculatedValue:

    /** EXERCISE 3
      *
      * Add a constructor that makes an CalculatedValue from a Value.
      */
    def const(contents: Value): CalculatedValue = Const(contents)

    /** EXERCISE 4
      *
      * Add a constructor that provides access to the value of the specified cell, identified by
      * col/row.
      */
    def at(col: Int, row: Int): CalculatedValue = At(col, row)

  // val cv: CalculatedValue = CalculatedValue.const(Value.Dbl(1.0)).sum(CalculatedValue.at(3, 2))

  // val ss = new SpreadSheet {
  //   val rows = 10
  //   val cols = 10
  //   def valueAt(3, 2) = CalculatedValue.const(Value.Dbl(3.0))
  // }

  // val nv = CalculatedValue.at(3, 2).negate // Not(At(3, 2))

  // val x = evaluate(ss, Cell(4, 4, nv))

  /** EXERCISE 5
    *
    * Implement an interpreter for the `Value.CalculatedValue` model that translates it into static
    * cell contents by evaluating the CalculatedValueession.
    */
  def negateValue(value: Value): Value = value match
    case Value.Error(message) => Value.Error(message)
    case Value.Str(value)     => Value.Str(value.reverse)
    case Value.Dbl(value)     => Value.Dbl(-value)

  def sumValue(value1: Value, value2: Value): Value = (value1, value2) match
    case (Value.Error(message1), Value.Error(message2)) =>
      Value.Error(message1 ++ " and " ++ message2)
    case (Value.Str(message1), Value.Str(message2))     => Value.Str(message1 ++ message2)
    case (Value.Dbl(value1), Value.Dbl(value2))         => Value.Dbl(value1 + value2)
    case _                                              => Value.Error("incomoapfmiamgpa")

  def evaluate(spreadsheet: Spreadsheet, cell: Cell): Value = cell.contents match
    case CalculatedValue.Sum(value1, value2) =>
      sumValue(
        evaluate(spreadsheet, cell.copy(contents = value1)),
        evaluate(spreadsheet, cell.copy(contents = value2))
      )
    case CalculatedValue.Not(value)          =>
      negateValue(evaluate(spreadsheet, cell.copy(contents = value)))
    case CalculatedValue.Const(contents)     =>
      contents
    case CalculatedValue.At(col, row)        =>
      evaluate(spreadsheet, Cell(col, row, spreadsheet.valueAt(col, row)))
end spreadsheet2

/** E-COMMERCE MARKETING - GRADUATION PROJECT
  *
  * Consider an e-commerce marketing platform where emails are sent to users whose history matches
  * specific patterns (for example, an event of adding a product to a shopping card, followed by an
  * abandonment of the web session).
  */
object ecommerce_marketing:
  type Event = Map[Attribute, Value]

  enum Attribute:
    case EventType
    case UserName
    case ShoppingCartId
    case Email
    case WebSession
    case DateTime

  enum Value:
    case Str(value: String)
    case Id(value: String)
    case Email(value: String)
    case DateTime(value: java.time.OffsetDateTime)

  object abstract_encoding:
    enum HistoryPattern:
      case Matches
      case EventP(eventPattern: EventPattern)
      case Sequence(first: HistoryPattern, second: HistoryPattern)
      case Repeat(pattern: HistoryPattern, min: Option[Int], max: Option[Int])

      def self = this

      def *>(that: HistoryPattern): HistoryPattern = HistoryPattern.Sequence(self, that)

      def atLeast(n: Int): HistoryPattern = repeat(Some(n), None)

      def atMost(n: Int): HistoryPattern = repeat(None, Some(n))

      def between(min: Int, max: Int): HistoryPattern = repeat(Some(min), Some(max))

      def repeat(min: Option[Int], max: Option[Int]): HistoryPattern =
        HistoryPattern.Repeat(self, min, max)
    object HistoryPattern:

      val matches: HistoryPattern = Matches

      def event(eventPattern: EventPattern): HistoryPattern = EventP(eventPattern)

      def eventType(eventType: String): HistoryPattern =
        event(EventPattern.HasValue(Attribute.EventType, Value.Str(eventType)))
    enum EventPattern:
      case Matches
      case HasValue(attr: Attribute, value: Value)

      def self = this

      import EventPattern.*

      def matches(event: Event): Boolean =
        self match
          case Matches               => true
          case HasValue(attr, value) => event.get(attr) == Some(value)

    import HistoryPattern.*
    import Attribute.EventType

    val example = eventType("add-item").atLeast(10) *> eventType("abandon-cart").between(10, 15)

    def matches(history: List[Event], pattern: HistoryPattern): Boolean =
      def loop(history: List[Event], pattern: HistoryPattern): (List[Event], Boolean) =
        (pattern, history.headOption) match
          case (EventP(eventPattern), Some(event)) => (history.tail, eventPattern.matches(event))
          case (EventP(_), None)                   => (history.tail, false)
          case (Sequence(first, second), _)        =>
            val (leftHistory, leftMatch) = loop(history, first)

            if leftMatch then loop(leftHistory, second) else (leftHistory, leftMatch)
          case (Repeat(pattern, min0, max0), _)    =>
            val min = min0.getOrElse(0)
            val max = max0.getOrElse(Int.MaxValue)

            val baseline = (0 to min).foldLeft((history, true)):
              case ((history, false), _) => (history, false)
              case ((history, true), _)  => loop(history, pattern)

            if !baseline._2 then baseline
            else
              val after = (0 to (max - min)).foldLeft(baseline):
                case ((history, false), _) => (history, false)
                case ((history, true), _)  => loop(history, pattern)

              (after._1, true)
          case _                                   => (history, false)
      loop(history, pattern)._2
    end matches
  end abstract_encoding

  /** EXERCISE 1
    *
    * Develop an executable encoding of the pattern matcher. Instead of using an ADT to represent a
    * pattern, and then interpreting that on a user history to see if there is a match, you will
    * represent a pattern as a function or an interface that is capable of testing the user history
    * for a match.
    */
  object executable_encoding:
    import abstract_encoding.EventPattern
    // enum HistoryPattern
    case class HistoryPattern(run: List[Event] => (List[Event], Boolean)):
      def *>(that: HistoryPattern): HistoryPattern                     = HistoryPattern: events =>
        run(events) match
          case (list, true)  => that.run(list)
          case (list, false) => (list, false)
      def atLeast(n: Int): HistoryPattern                              = repeat(Some(n), None)
      def atMost(n: Int): HistoryPattern                               = repeat(None, Some(n))
      def between(min: Int, max: Int): HistoryPattern                  = repeat(Some(min), Some(max))
      def repeat(min0: Option[Int], max0: Option[Int]): HistoryPattern =
        HistoryPattern: events =>
          val min      = min0.getOrElse(0)
          val max      = max0.getOrElse(Int.MaxValue)
          val baseline = (0 to min).foldLeft((events, true)):
            case ((history, false), _) => (history, false)
            case ((history, true), _)  => run(history)

          if !baseline._2 then baseline
          else
            val after = (0 to (max - min)).foldLeft(baseline):
              case ((history, false), _) => (history, false)
              case ((history, true), _)  => run(history)

            (after._1, true)

      def matches(events: List[Event]): Boolean = run(events)._2
    end HistoryPattern

    object HistoryPattern:
      val matches: HistoryPattern                           = HistoryPattern(events => (events, true))
      def event(eventPattern: EventPattern): HistoryPattern =
        HistoryPattern:
          case head :: next => (next, eventPattern.matches(head))
          case Nil          => (Nil, false)

      def eventType(eventType: String): HistoryPattern = event(
        EventPattern.HasValue(Attribute.EventType, Value.Str(eventType))
      )
    end HistoryPattern

    import HistoryPattern.*

    val example             = eventType("add-item").atLeast(10) *> eventType("abandon-cart").between(10, 15)
    val events: List[Event] = ???
    val x                   = example.matches(events)
    // Instead of having an ADT to represent a pattern
  end executable_encoding
end ecommerce_marketing
