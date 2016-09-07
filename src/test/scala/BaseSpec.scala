import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.PropertyChecks
import org.scalatest.{EitherValues, Matchers, OptionValues, WordSpec}

trait BaseSpec extends WordSpec with PropertyChecks with Matchers with OptionValues with EitherValues with ScalaFutures
