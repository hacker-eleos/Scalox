package com.craftinginterpreters.scalox

class Scanner(source: String) {
  def scanTokens(): List[Token] | Unit = {
    try {
      val lines = source.linesIterator;
      lines.zip(1 until lines.length).flatMap((line: String, i: Int) => {
        line.map[Token] {
          case '(' => Token(TokenType.LEFT_PAREN, "(", (), i)
          case ')' => Token(TokenType.RIGHT_PAREN, ")", (), i)
          case '{' => Token(TokenType.LEFT_BRACE, "{", (), i)
          case '}' => Token(TokenType.RIGHT_BRACE, "}", (), i)
          case ',' => Token(TokenType.COMMA, ",", (), i)
          case '.' => Token(TokenType.DOT, ".", (), i)
          case '-' => Token(TokenType.MINUS, ".", (), i)
          case '+' => Token(TokenType.PLUS, ".", (), i)
          case ';' => Token(TokenType.SEMICOLON, ".", (), i)
          case '*' => Token(TokenType.STAR, ".", (), i)
          case _ => throw Exception(s"line:${i}")
        }
      }).toList
    } catch {
      case e: Exception => {
        val lineNumber = e.getMessage.split(":").apply(1).toInt
        Scalox.error(lineNumber, "Unexpected string")
      }
    }
  }
}