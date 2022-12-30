package com.craftinginterpreters.scalox

import scala.annotation.tailrec

class Scanner(source: String) {
  def scanTokens(): List[Token] | Unit = {
    @tailrec
    def scanTokens(currentIndex: Int, lineNumber: Int, collectedTokens: List[Token]): List[Token] | Unit = {
      if (currentIndex >= source.length) return collectedTokens

      def isMatchNextChar(char: Char): Boolean = {
        if (currentIndex.equals(source.length - 1)) return false;
        if (source(currentIndex).equals(char)) return true;
        false
      }

      val currentChar = source.apply(currentIndex);
      currentChar match {
        case '(' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.LEFT_PAREN, "(", (), lineNumber) :: collectedTokens)
        case ')' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.RIGHT_PAREN, ")", (), lineNumber) :: collectedTokens)
        case '{' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.LEFT_BRACE, "{", (), lineNumber) :: collectedTokens)
        case '}' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.RIGHT_BRACE, "}", (), lineNumber) :: collectedTokens)
        case ',' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.COMMA, ",", (), lineNumber) :: collectedTokens)
        case '.' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.DOT, ".", (), lineNumber) :: collectedTokens)
        case '-' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.MINUS, ".", (), lineNumber) :: collectedTokens)
        case '+' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.PLUS, ".", (), lineNumber) :: collectedTokens)
        case ';' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.SEMICOLON, ".", (), lineNumber) :: collectedTokens)
        case '*' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.STAR, ".", (), lineNumber) :: collectedTokens)
        case '!' =>
          if (isMatchNextChar('=')) scanTokens(currentIndex + 2, lineNumber, Token(TokenType.BANG_EQUAL, "!=", (), lineNumber) :: collectedTokens)
          else scanTokens(currentIndex + 1, lineNumber, Token(TokenType.BANG, "!", (), lineNumber) :: collectedTokens)
        case '=' =>
          if (isMatchNextChar('=')) scanTokens(currentIndex + 2, lineNumber, Token(TokenType.EQUAL_EQUAL, "==", (), lineNumber) :: collectedTokens)
          else scanTokens(currentIndex + 1, lineNumber, Token(TokenType.EQUAL, "=", (), lineNumber) :: collectedTokens)
        case '<' =>
          if (isMatchNextChar('=')) scanTokens(currentIndex + 2, lineNumber, Token(TokenType.LESS_EQUAL, "<=", (), lineNumber) :: collectedTokens)
          else scanTokens(currentIndex + 1, lineNumber, Token(TokenType.EQUAL, "<", (), lineNumber) :: collectedTokens)
        case '>' =>
          if (isMatchNextChar('=')) scanTokens(currentIndex + 2, lineNumber, Token(TokenType.GREATER_EQUAL, ">=", (), lineNumber) :: collectedTokens)
          else scanTokens(currentIndex + 1, lineNumber, Token(TokenType.EQUAL, "=", (), lineNumber) :: collectedTokens)
        case '/' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.SLASH, "/", (), lineNumber) :: collectedTokens)
        case _ => Scalox.error(lineNumber, "Unexpected string")
      }
    }

    scanTokens(0, 0, Nil);
  }
}