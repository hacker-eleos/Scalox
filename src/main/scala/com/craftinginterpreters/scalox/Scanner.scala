package com.craftinginterpreters.scalox

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

class Scanner(final val source: String) {
  private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def isAlphabet(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  private val keywords = HashMap[String, TokenType](
    ("and", TokenType.AND),
    ("class", TokenType.CLASS),
    ("else", TokenType.ELSE),
    ("false", TokenType.FALSE),
    ("for", TokenType.FOR),
    ("fun", TokenType.FUN),
    ("if", TokenType.IF),
    ("nil", TokenType.NIL),
    ("or", TokenType.OR),
    ("print", TokenType.PRINT),
    ("return", TokenType.RETURN),
    ("super", TokenType.SUPER),
    ("this", TokenType.THIS),
    ("true", TokenType.TRUE),
    ("var", TokenType.VAR),
    ("while", TokenType.WHILE))

  @tailrec
  private def scanTokens(currentIndex: Int, lineNumber: Int, collectedTokens: List[Token]): List[Token] | Unit = {
    if (currentIndex >= source.length) return collectedTokens

    def isMatchNextChar(char: Char): Boolean = {
      if (currentIndex.equals(source.length - 1)) return false
      if (source(currentIndex).equals(char)) return true
      false
    }

    val currentChar = source.charAt(currentIndex)
    currentChar match {
      case '(' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.LEFT_PAREN, "(", null, lineNumber) :: collectedTokens)
      case ')' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.RIGHT_PAREN, ")", null, lineNumber) :: collectedTokens)
      case '{' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.LEFT_BRACE, "{", null, lineNumber) :: collectedTokens)
      case '}' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.RIGHT_BRACE, "}", null, lineNumber) :: collectedTokens)
      case ',' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.COMMA, ",", null, lineNumber) :: collectedTokens)
      case '.' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.DOT, ".", null, lineNumber) :: collectedTokens)
      case '-' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.MINUS, "-", null, lineNumber) :: collectedTokens)
      case '+' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.PLUS, "+", null, lineNumber) :: collectedTokens)
      case ';' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.SEMICOLON, ";", null, lineNumber) :: collectedTokens)
      case '*' => scanTokens(currentIndex + 1, lineNumber, Token(TokenType.STAR, "*", null, lineNumber) :: collectedTokens)
      case '!' =>
        if (isMatchNextChar('=')) scanTokens(currentIndex + 2, lineNumber, Token(TokenType.BANG_EQUAL, "!=", null, lineNumber) :: collectedTokens)
        else scanTokens(currentIndex + 1, lineNumber, Token(TokenType.BANG, "!", null, lineNumber) :: collectedTokens)
      case '=' =>
        if (isMatchNextChar('=')) scanTokens(currentIndex + 2, lineNumber, Token(TokenType.EQUAL_EQUAL, "==", null, lineNumber) :: collectedTokens)
        else scanTokens(currentIndex + 1, lineNumber, Token(TokenType.EQUAL, "=", null, lineNumber) :: collectedTokens)
      case '<' =>
        if (isMatchNextChar('=')) scanTokens(currentIndex + 2, lineNumber, Token(TokenType.LESS_EQUAL, "<=", null, lineNumber) :: collectedTokens)
        else scanTokens(currentIndex + 1, lineNumber, Token(TokenType.EQUAL, "<", null, lineNumber) :: collectedTokens)
      case '>' =>
        if (isMatchNextChar('=')) scanTokens(currentIndex + 2, lineNumber, Token(TokenType.GREATER_EQUAL, ">=", null, lineNumber) :: collectedTokens)
        else scanTokens(currentIndex + 1, lineNumber, Token(TokenType.EQUAL, "=", null, lineNumber) :: collectedTokens)
      case '/' =>
        if (isMatchNextChar('/'))
          val endOfLineIndex = source.indexOf('\n', currentIndex)
          if(endOfLineIndex == -1) Token(TokenType.COMMENT,"//", source.substring(currentIndex+2), lineNumber)::collectedTokens
          else scanTokens(currentIndex+endOfLineIndex, lineNumber+1, Token(TokenType.COMMENT,"//", source.substring(currentIndex+2, endOfLineIndex), lineNumber)::collectedTokens)
        else scanTokens(currentIndex + 1, lineNumber, Token(TokenType.SLASH, "/", null, lineNumber) :: collectedTokens)

      case ' ' => scanTokens(currentIndex + 1, lineNumber, collectedTokens)
      case '\r' => scanTokens(currentIndex + 1, lineNumber, collectedTokens)
      case '\t' => scanTokens(currentIndex + 1, lineNumber, collectedTokens)
      case '\n' => scanTokens(currentIndex + 1, lineNumber + 1, collectedTokens)
      case '"' =>
        val endOfStringIndex = source.indexOf('"', currentIndex + 1)
        if (endOfStringIndex == -1) Scalox.error(lineNumber, "Unterminated string")
        val extractedString = source.substring(currentIndex + 1, endOfStringIndex)
        val numberOfNewLines = extractedString.count(_ == '\n')
        scanTokens(endOfStringIndex + 1, lineNumber + numberOfNewLines, Token(TokenType.STRING, extractedString, null, lineNumber) :: collectedTokens)
      case c if isDigit(c) =>
      case c if isAlphabet(c) =>
        val identifier = source.substring(currentIndex).takeWhile(c => isAlphabet(c) || isDigit(c))
        val tokenType = keywords.get(identifier)
        tokenType match {
          case Some(x) => scanTokens(currentIndex + identifier.length, lineNumber, Token(x, identifier, null, lineNumber) :: collectedTokens)
          case None => scanTokens(currentIndex + identifier.length, lineNumber, Token(TokenType.IDENTIFIER, identifier, null, lineNumber) :: collectedTokens)
        }
      case _ => Scalox.error(lineNumber, "Unexpected string")
    }
  }

  def scanTokens(): List[Token] = {
    val tokens = scanTokens(0, 0, Nil)
    tokens match {
      case t: List[Token] => t
      case _: Unit => Nil
    }
  }
}