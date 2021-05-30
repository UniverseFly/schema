package schema

import semantics.Environment.emptyEnvironment
import Console.flush
import io.StdIn.readLine

object REPL {
  def apply(prompt: String = "schema> ") = {
    val env = Builtins.stdEnv
    while (true) {
      print(prompt)
      flush()
      val expr = Parser(readLine())
      println(eval(expr, env))
    }
  }
}
