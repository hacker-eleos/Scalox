package com.craftinginterpreters.scalox

import java.nio.charset.Charset
import scala.io.{Codec, Source, StdIn}

object Scalox {

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      Console.println("Usage: scalox [script]")
      sys.exit(64);
    } else if (args.length == 1) {
      runFile(args(0));
    } else {
      runPrompt()
    }
  }

  def runFile(path: String): Unit = {
    val bufferedReader = Source.fromFile(path)(new Codec(Charset.defaultCharset())).bufferedReader();
    run(bufferedReader.toString);
    bufferedReader.close();
  }

  def runPrompt(): Unit = {
    Iterator.continually(StdIn.readLine).takeWhile(_ != null).foreach(line => run(line));
  }

  def run(source: String): Unit = {
    Console.println(source)
//    Scanner scanner = new Scanner(source)
//    val tokens: List[Token] = scanner.scanTokens()
//    tokens.foreach(Console.println)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def report(line: Int, where: String, message: String): Unit = {
    Console.err.println("[line " + line + "] Error" + where + ": " + message)
  }



}
