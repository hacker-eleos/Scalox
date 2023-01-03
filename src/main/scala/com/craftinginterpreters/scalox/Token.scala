package com.craftinginterpreters.scalox

case class Token(final val tokenType: TokenType, final val lexeme: String, final val literal: AnyRef, final val line: Int) {
  override def toString: String = tokenType.toString + " " + lexeme + " " + literal + " line: " + line
}