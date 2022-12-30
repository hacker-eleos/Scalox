package com.craftinginterpreters.scalox

import java.nio.charset.Charset
import scala.io.{Codec, Source, StdIn}
import java.io.BufferedReader

object Scalox {

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      Console.println("Usage: scalox [script]")
      sys.exit(64)
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }

  def runFile(path: String): Unit = {
    val source = Source.fromFile(path)
    run(try source.mkString finally source.close())
  }

  def runPrompt(): Unit = {
    Iterator.continually(StdIn.readLine).takeWhile(_ != null).foreach(line => run(line))
  }

  def run(source: String): Unit = {
    val scanner = new Scanner(source)
    val tokens: List[Token] = scanner.scanTokens()
    tokens.foreach(Console.println)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def report(line: Int, where: String, message: String): Unit = {
    Console.err.println("[line " + line + "] Error" + where + ": " + message)
  }

}
