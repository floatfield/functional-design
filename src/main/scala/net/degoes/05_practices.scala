package net.degoes

import scala.math.{ cos, sin }

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.
 *
 */

/** ORTHOGONALITY - EXERCISE SET 1
  */
object email_filter3:
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /** EXERCISE 1
    *
    * In the following model, which describes an email filter, there are many primitives with
    * overlapping responsibilities. Find the smallest possible set of primitive operators and
    * constructors, without deleting any constructors or operators (you may implement them in terms
    * of primitives).
    *
    * NOTE: You may *not* use a final encoding, which would allow you to collapse everything down to
    * one primitive.
    */
  enum EmailFilter:
    case Always
    case And(left: EmailFilter, right: EmailFilter)
    case InclusiveOr(left: EmailFilter, right: EmailFilter)
    case RecipientEquals(target: Address)
    case SenderIn(targets: Set[Address])
    case BodyContains(phrase: String)
    case SubjectContains(phrase: String)
    case Not(filter: EmailFilter)

    def self = this

    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ^^(that: EmailFilter): EmailFilter =
      EmailFilter.InclusiveOr(
        EmailFilter.And(self, EmailFilter.Not(that)),
        EmailFilter.And(that, EmailFilter.Not(self))
      )

    def ||(that: EmailFilter): EmailFilter = EmailFilter.InclusiveOr(self, that)
  end EmailFilter
  object EmailFilter:
    val always: EmailFilter = Always

    val never: EmailFilter = EmailFilter.Not(Always)

    def senderIs(sender: Address): EmailFilter = SenderIn(Set(sender))

    def senderIsNot(sender: Address): EmailFilter = Not(SenderIn(Set(sender)))

    def recipientIs(recipient: Address): EmailFilter = RecipientEquals(recipient)

    def recipientIsNot(recipient: Address): EmailFilter = Not(RecipientEquals(recipient))

    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    def recipientIn(recipients: Set[Address]): EmailFilter =
      recipients.foldLeft(never)((res, recipient) => RecipientEquals(recipient) || res)

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = Not(BodyContains(phrase))

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = Not(SubjectContains(phrase))
  end EmailFilter
end email_filter3

/** COMPOSABILITY - EXERCISE SET 2
  */
object ui_components:

  val r = 10

  object executable:
    /** EXERCISE 1
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use an executable model.
      */

    trait DrawApi:
      def draw(x: Coord, y: Coord): Unit

    final case class Coord(x: Double, y: Double)

    final case class TurtleState(point: Coord, alphaTurn: Int, isDrawing: Boolean)

    case class TurtleExec(run: TurtleState => TurtleState)(using api: DrawApi):
      def andThen(that: TurtleExec): TurtleExec = TurtleExec { state =>
        that.run(run(state))
      }
      def turnLeft(degrees: Int): TurtleExec    = TurtleExec { state =>
        state.copy(alphaTurn = (state.alphaTurn - degrees) % 360)
      }

      def goForward(): TurtleExec = TurtleExec { state =>
        val newState = state.copy(point =
          Coord(state.point.x + r * cos(state.alphaTurn), state.point.y + r * sin(state.alphaTurn))
        )
        if state.isDrawing then api.draw(state.point, newState.point)
        newState
      }

    object TurtleExec:
      def init(using api: DrawApi): TurtleExec = TurtleExec(identity)

    class DrawApiImpl extends DrawApi:
      override def draw(x: Coord, y: Coord): Unit = ()

    val x = TurtleExec.init(using DrawApiImpl())

    val transformation = x.turnLeft(30).goForward().turnLeft(20).turnLeft(50)
    val y              = transformation.andThen(transformation)
    y.run(TurtleState(Coord(0, 3), 0, false))

    trait Turtle:
      self =>
      def turnLeft(degrees: Int): Unit

      def turnRight(degrees: Int): Unit

      def goForward(): Unit

      def goBackward(): Unit

      def draw(): Unit
  end executable

  object declarative:
    /** EXERCISE 2
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use a declarative model.
      */
    trait Turtle:
      self =>
      def turnLeft(degrees: Int): Unit

      def turnRight(degrees: Int): Unit

      def goForward(): Unit

      def goBackward(): Unit

      def draw(): Unit
end ui_components
