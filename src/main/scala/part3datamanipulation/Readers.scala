package part3datamanipulation

object Readers {

  /*
    We have a multi-layers application
    configuration file => initial data structure for:
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */

  // overall config
  case class Configuration(
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      nThreads: Int,
      emailReplyTo: String
  )
  // bootstrap
  val config = Configuration(
    "daniel",
    "rockthejvm1!",
    "localhost",
    1234,
    8,
    "daniel@rockthejvm.com"
  )

  // DB layer
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String =
      "dispatched" // select * from the db table and return the status of the orderID
    def getLastOrderId(username: String): Long =
      542643 // select max(orderId) from table where username = username
  }
  // HTTP layer
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println(
      "server started"
    ) // this would start the actual server
  }

  //
  // cats Reader
  import cats.data.Reader

  // given a configuration, the reader is able to extract a DB connection
  // it is a function Configuration => DbConnection
  val dbReader: Reader[Configuration, DbConnection] =
    Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config) // inject the config object

  // Reader[Input, Output]: we derive a new reader from a previous one
  // Oss: the dependency needed is still a Configuration
  // We have just composed a Configuration => DbConnection with a DbConnection => String
  val danielsOrderStatusReader: Reader[Configuration, String] =
    dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val danielsOrderStatus: String = danielsOrderStatusReader.run(
    config
  ) // <- overall config is needed to run dbReader first

  /*
    Pattern
    1. you create the initial data structure
    2. you create a reader which specifies how that data structure will be manipulated later
    3. you can then map & flatMap the reader to produce derived information
    4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  // An example: fetch the last order swtatus given an username
  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    // identical:
    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }

  //
  //
  // TODO
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) =
      s"From: $emailReplyTo; to: $address >>> $contents"
  }

  def emailUser(username: String, userEmail: String) = {
    // fetch the status of their last order
    // email them with the Email service: "Your last order has the status: (status)"

    val status = getLastOrderStatus(username)

    val emailServiceReader: Reader[Configuration, EmailService] =
      Reader(conf => EmailService(conf.emailReplyTo))

    val emailReader = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader

    } yield emailService.sendEmail(
      userEmail,
      s"Your last order has the status: $orderStatus"
    )

    emailReader.run(config)
  }

  //
  // TODO 2: what programming pattern do Readers remind you of?
  // Dependency injection!

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("daniel", "daniel@rtjvm.com"))
  }
}
