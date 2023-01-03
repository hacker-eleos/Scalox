import org.scalatest._
import funspec._
import matchers._
import com.craftinginterpreters.scalox.Scanner as ScannerSpec
class Scanner extends AnyFunSpec{

  describe("Valid programs"){
    it("should have 14 tokens"){
      val program =
        """var p = 23.34
          // this is a comment
          ((2+3) == 5)"""

      val scanner = new ScannerSpec(program)
      val tokens = scanner.scanTokens()
      assert(tokens.nonEmpty)
      assert(tokens.length == 14)
      tokens.foreach(Console.println)
    }
    it("should have 4 tokens") {
      val program =
        """var p = 68758.3451345235"""

      val scanner = new ScannerSpec(program)
      val tokens = scanner.scanTokens()
      assert(tokens.nonEmpty)
      assert(tokens.length == 4)
    }
  }
}
