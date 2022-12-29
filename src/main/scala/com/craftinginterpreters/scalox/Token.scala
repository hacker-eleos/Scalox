package com.craftinginterpreters.scalox

import TokenType.Token

class Token(tokenType: Token, lexeme: String, literal: AnyVal, line: Int) {
  override def toString: String = tokenType + " " + lexeme + " " + literal
}
