package com.craftinginterpreters.scalox

import TokenType._

class Token(tokenType: TokenType, lexeme: String, literal: AnyVal, line: Int) {
  override def toString: String = tokenType + " " + lexeme + " " + literal
}