package com.craftinginterpreters.scalox

import TokenType._

class Token(final val tokenType: TokenType, final val lexeme: String, final val literal: AnyVal, final val line: Int) {
  override def toString: String = tokenType.toString + " " + lexeme + " " + literal
}