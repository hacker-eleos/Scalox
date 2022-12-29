package com.craftinginterpreters.scalox
import TokenType

class Scanner(source: String) {
  def scanTokens(): List[Token] = {
    source.zip(0 until source.length)
  }
}
