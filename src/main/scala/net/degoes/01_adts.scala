package net.degoes

import java.time.Instant
import java.time.Duration

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/** E-COMMERCE - EXERCISE SET 1
  *
  * Consider an e-commerce application that allows users to purchase products.
  */
object credit_card:

  /** EXERCISE 1
    *
    * Using only enums and case classes, create an immutable data model of a credit card, which must
    * have:
    *
    * * Number * Name * Expiration date * Security code
    */
  opaque type CreditCardNumber = String

  object CreditCardNumber:
    def fromString(number: String): Option[CreditCardNumber] = Some(number)

  opaque type Name = String

  object Name:
    def fromString(name: String): Option[Name] = Some(name)

  opaque type SecurityCode = Int

  case class CreditCard(
    number: CreditCardNumber,
    name: Name,
    date: java.util.Date,
    securityCode: SecurityCode
  )

  /** EXERCISE 2
    *
    * Using only enums and case classes, create an immutable data model of a product, which could be
    * a physical product, such as a gallon of milk, or a digital product, such as a book or movie,
    * or access to an event, such as a music concert or film showing.
    */
  enum Product:
    case Phisycal, Digital, EventAccess

  /** EXERCISE 3
    *
    * Using only enums and case classes, create an immutable data model of a product price, which
    * could be one-time purchase fee, or a recurring fee on some regular interval.
    */
  type Price

  enum PricingScheme(val fee: Price):
    case Once(override val fee: Price)                             extends PricingScheme(fee)
    case Reccuring(override val fee: Price, inteval: zio.Duration) extends PricingScheme(fee)

end credit_card

/** EVENT PROCESSING - EXERCISE SET 3
  *
  * Consider an event processing application, which processes events from both devices, as well as
  * users.
  */
object events:

  /** EXERCISE
    *
    * Refactor the object-oriented data model in this section to a more functional one, which uses
    * only enums and case classes.
    */
  enum Event(val id: Int, val time: Instant):
    case UserEvent[T](
      override val id: Int,
      override val time: Instant,
      userName: String,
      payload: T
    ) extends Event(id, time)
    case DeviceEvent[T](override val id: Int, override val time: Instant, deviceId: Int, payload: T)
        extends Event(id, time)

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  // trait UserEvent extends Event:
  //   def userName: String

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  // trait DeviceEvent extends Event:
  //   def deviceId: Int

  // class SensorUpdated(id: Int, val deviceId: Int, val time: Instant, val reading: Option[Double])
  //     extends Event(id)
  //     with DeviceEvent
  type Reading       = Option[Double]
  type SensorUpdated = Event.DeviceEvent[Reading]

  extension (sensor: SensorUpdated) def reading: Option[Double] = sensor.payload

  object SensorUpdated:
    def apply(id: Int, time: Instant, deviceId: Int, reading: Option[Double]): SensorUpdated =
      Event.DeviceEvent(id, time, deviceId, reading)

  // val su = SensorUpdated(4, Instant.now(), 4, Some(44.4))
  // su.reading

  // case class SensorUpdated(event: Event.DeviceEvent, reading: Option[Double])

  // object SensorUpdated {
  //   def apply()
  // }

  type DeviceActivated = Event.DeviceEvent[Unit]

  object DeviceActivated:
    def apply(id: Int, time: Instant, deviceId: Int): DeviceActivated =
      Event.DeviceEvent(id, time, deviceId, ())

  // class UserPurchase(
  // id: Int,
  // val item: String,
  // val price: Double,
  // val time: Instant,
  // val userName: String
  // ) extends Event(id)
  //     with UserEvent

  case class Purchase(val item: String, val price: Double)

  type UserPurchase = Event.UserEvent[Purchase]

  object UserPurchase:
    def apply(id: Int, item: String, price: Double, time: Instant, userName: String): UserPurchase =
      Event.UserEvent(id, time, userName, Purchase(item, price))

  // case class UserPurchase(event: Event.UserEvent, val item: String, val price: Double)

  // class UserAccountCreated(id: Int, val userName: String, val time: Instant)
  //     extends Event(id)
  //     with UserEvent
  // class UserAccountCreated(event: Event.UserEvent)

  type UserAccountCreated = Event.UserEvent[Unit]
  object UserAccountCreated:
    def apply(id: Int, time: Instant, userName: String): UserAccountCreated =
      Event.UserEvent(id, time, userName, ())
end events

/** DOCUMENT EDITING - EXERCISE SET 4
  *
  * Consider a web application that allows users to edit and store documents of some type (which is
  * not relevant for these exercises).
  */
object documents:
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /** EXERCISE 1
    *
    * Using only enums and case classes, create a simplified but somewhat realistic model of a
    * Document.
    */
  case class Document(id: DocId, content: DocContent)

  /** EXERCISE 2
    *
    * Using only enums and case classes, create a model of the access type that a given user might
    * have with respect to a document. For example, some users might have read-only permission on a
    * document.
    */
  enum AccessType(val docId: DocId, val userId: UserId):
    case Read(override val docId: DocId, override val userId: UserId)
        extends AccessType(docId, userId)
    case Edit(override val docId: DocId, override val userId: UserId)
        extends AccessType(docId, userId)

  /** EXERCISE 3
    *
    * Using only enums and case classes, create a model of the permissions that a user has on a set
    * of documents they have access to. Do not store the document contents themselves in this model.
    */
//  case class DocPermission(docId: DocId, userId: UserId, access: AccessType)

  case class DocPermissions(perms: Vector[AccessType])
end documents

/** BANKING - EXERCISE SET 5
  *
  * Consider a banking application that allows users to hold and transfer money.
  */
object bank:

  /** EXERCISE 1
    *
    * Using only enums and case classes, develop a model of a customer at a bank.
    */
  case class Customer(id: String)

  /** EXERCISE 2
    *
    * Using only enums and case classes, develop a model of an account type. For example, one
    * account type allows the user to write checks against a given currency. Another account type
    * allows the user to earn interest at a given rate for the holdings in a given currency.
    */
  type Currency
  type Holding

  case class Interest(holding: Holding, currency: Currency, interestRate: Double)

  enum AccountType:
    case Checker(currency: Currency)
    case Earner(interests: List[Interest])

  /** EXERCISE 3
    *
    * Using only enums and case classes, develop a model of a bank account, including details on the
    * type of bank account, holdings, customer who owns the bank account, and customers who have
    * access to the bank account.
    */
  case class Account(
    owner: Customer,
    accountType: AccountType,
    accessments: Seq[Customer],
    holdings: Seq[Holding]
  )
end bank

/** STOCK PORTFOLIO - GRADUATION PROJECT
  *
  * Consider a web application that allows users to manage their portfolio of investments.
  */
object portfolio:

  /** EXERCISE 1
    *
    * Using only enums and case classes, develop a model of a stock exchange. Ensure there exist
    * values for NASDAQ and NYSE.
    */
  enum Exchange:
    case NASDAQ, NYSE

  /** EXERCISE 2
    *
    * Using only enums and case classes, develop a model of a currency type.
    */
  enum CurrencyType:
    case USD, EUR, ETH

  /** EXERCISE 3
    *
    * Using only enums and case classes, develop a model of a stock symbol. Ensure there exists a
    * value for Apple's stock (APPL).
    */
  enum StockSymbol:
    case APPL, NVDA

  /** EXERCISE 4
    *
    * Using only enums and case classes, develop a model of a portfolio held by a user of the web
    * application.
    */
  case class Portfolio(id: String, exchange: Exchange, stocks: Map[StockSymbol, Int])

  /** EXERCISE 5
    *
    * Using only enums and case classes, develop a model of a user of the web application.
    */
  case class User(id: String, name: String)

  /** EXERCISE 6
    *
    * Using only enums and case classes, develop a model of a trade type. Example trade types might
    * include Buy and Sell.
    */
  enum TradeType:
    case Buy, Sell

  /** EXERCISE 7
    *
    * Using only enums and case classes, develop a model of a trade, which involves a particular
    * trade type of a specific stock symbol at specific prices.
    */
  case class Trade(
    tradeType: TradeType,
    stock: StockSymbol,
    price: Double,
    currency: CurrencyType,
    portfolio: Portfolio
  )
end portfolio
