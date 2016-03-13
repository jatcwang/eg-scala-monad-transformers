import scalaz._
import scalaz.syntax.either._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scalaz.std.scalaFuture._

object Main {
  def main(args: Array[String]): Unit = {
    println(s"Trying to make parcel for Maggie (id: 0)... ${makeParcel("0")}")
    println(s"Trying to make parcel for John (id: 1)... ${makeParcel("1")}")
    println(s"Trying to make parcel for Tim (id: 2)... ${makeParcel("2")}")

    println(s"Making parcel for Maggie using remote services... ${Await.result(makeParcelMegaCorp("0"), 1.second)}")
    println(s"Making parcel for John using remote services... ${Await.result(makeParcelMegaCorp("1"), 1.second)}")
    println(s"Making parcel for Tim using remote services... ${Await.result(makeParcelMegaCorp("2"), 1.second)}")

    println(s"Making parcel for Maggie using remote services... ${Await.result(makeParcelMegaCorp_improved("0"), 1.second)}")
    println(s"Making parcel for John using remote services... ${Await.result(makeParcelMegaCorp_improved("1"), 1.second)}")
    println(s"Making parcel for Tim using remote services... ${Await.result(makeParcelMegaCorp_improved("2"), 1.second)}")

    import scalaz._, Scalaz._
    type ErrorT[M[_], A] = EitherT[M, String, A]
    type FutureOpt[A] = OptionT[Future, A]
    val t: FutureOpt[String \/ Int] = OptionT(1.right[String].point[Option].point[Future])
    val v: ErrorT[FutureOpt, Int] = EitherT(t)
  }

  def makeParcel(userId: String): \/[SendParcelError, Parcel] = {
    import ParcelMaker._
    for {
      user <- getUser(userId)
      address <- getAddress(user)
      mobileNumber <- getMobileNumber(user)
    } yield Parcel(user.name, address, mobileNumber)
  }

  def makeParcelMegaCorp(userId: String): Future[\/[SendParcelError, Parcel]] = {
    import ParcelMakerMegaCorp._
    for {
      userOrError: \/[SendParcelError, User] <- remote_getUser(userId)
      addressOrError: \/[SendParcelError, String] <- userOrError match {
        case \/-(user) => remote_getAddress(user)
        case -\/(error) => Future.successful(error.left)
      }
      mobileNumberOrError: \/[SendParcelError, String] <- userOrError match {
        case \/-(user) => remote_getMobileNumber(user)
        case -\/(error) => Future.successful(error.left)
      }

    } yield for {
      user <- userOrError
      address <- addressOrError
      mobileNumber <- mobileNumberOrError
    } yield  Parcel(user.name, address, mobileNumber)
  }

  def makeParcelMegaCorp_improved(userId: String): Future[\/[SendParcelError, Parcel]] = {
    import ParcelMakerMegaCorp._
    val transformer = for {
      user <- EitherT(remote_getUser(userId))
      address <- EitherT(remote_getAddress(user))
      mobileNumber <- EitherT(remote_getAddress(user))
    } yield Parcel(user.name, address, mobileNumber)
    transformer.run
  }
}

sealed trait SendParcelError
case object UserNotFound extends SendParcelError
case object ExpiredUser extends SendParcelError
case object NoAddress extends SendParcelError
case object NoMobileNumber extends SendParcelError

case class User(userId: String, name: String)
case class Parcel(name: String, address: String, mobileNumber: String)

object ParcelMaker {

  def getUser(userId: String): \/[SendParcelError, User] = {
    userId match {
      case "0" => User("0", "Maggie").right
      case "1" => User("1", "John").right
      case "2" => User("2", "Tim").right
      case _ => UserNotFound.left
    }
  }

  def getAddress(user: User): \/[SendParcelError, String] = {
    user.userId match {
      case "0" => "123 Sesame St".right
      case "1" => "55 English St".right
      case _ => NoAddress.left
    }
  }

  def getMobileNumber(user: User): \/[SendParcelError, String] = {
    user.userId match {
      case "1" => "123456".right
      case _ => NoMobileNumber.left
    }
  }
}

object ParcelMakerMegaCorp {
  import ParcelMaker.{getUser, getAddress, getMobileNumber}

  def remote_getUser(userId: String): Future[\/[SendParcelError, User]] = {
    Future {
      Thread.sleep(50)
      getUser(userId)
    }
  }

  def remote_getAddress(user: User): Future[\/[SendParcelError, String]] = {
    Future {
      Thread.sleep(50)
      getAddress(user)
    }
  }

  def remote_getMobileNumber(user: User): Future[\/[SendParcelError, String]] = {
    Future {
      Thread.sleep(50)
      getMobileNumber(user)
    }
  }

}
