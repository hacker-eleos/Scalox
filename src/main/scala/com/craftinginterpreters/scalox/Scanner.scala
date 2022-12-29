package com.craftinginterpreters.scalox

class Scanner(source: String) {
  def scanTokens(): List[Token] = {
    val lines = source.linesIterator;
    lines.zip(1 until lines.length).flatMap((line, i) => {
      line.map {
        case '(' => List(new Token(TokenType.LEFT_PAREN, "(", (), i ))
        case _ => Scalox.error(i, "Unexpected string")
      }
    })

  }
}
