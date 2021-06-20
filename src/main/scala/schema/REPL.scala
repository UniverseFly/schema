package schema

import semantics.Environment.emptyEnvironment
import Console.flush
import io.StdIn.readLine

object REPL {
  val shouldExit = (s: String) => s == null || s == "exit" || s == "quit"
  val shouldSkip = (s: String) => s == ""

  def apply(prompt: String = "schema> ") = {
    val env = Builtins.stdEnv
    var done = false
    println("Welcome to schema REPL, created by Yuxiang in 2021!")
    while (!done) {
      try {
        print(prompt)
        flush()
        val input = readLine()
        if shouldExit(input) then {
          println(f"Bye!")
          done = true
        } else if shouldSkip(input) then ()
        else {
          val expr = Parser(input)
          println(eval(expr, env))
        }
      } catch {
        case e => System.err.println(e)
      }
    }
  }
}
