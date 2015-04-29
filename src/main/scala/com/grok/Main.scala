package com.grok

import java.io.File

/**
 * Created by brendan.
 */
object Main {
  def main(args: Array[String]): Unit = {
    val (flags, sources) = parseFlags(args)
    sources.foreach(validateSource)

    val compiler = CompilerFactory.compiler(flags)
    compiler.compile(sources)
  }

  val version = "0.1.2"

  def parseFlags(args: Seq[String]): (Flags, Seq[String]) = {
    val flags = new Flags
    val parseFlag: PartialFunction[(String, String), Boolean] = {
      case ("--compiler_type", value) => flags.compilerType = parseCompilerType(value); true
      case ("--version", "") => println(version); sys.exit(0)
      case _ => false
    }
    (flags, args.filterNot(arg => parseFlag(splitFlag(arg))))
  }

  def validateSource(source: String): Unit = {
    if (!new File(source).exists()) {
      sys.error(source + ": does not exist")
    }
  }

  def splitFlag(arg: String): (String, String) = {
    val left :: right :: _ = arg.split("=", 2).padTo(2, "").toList
    (left, right)
  }

  val parseCompilerType: PartialFunction[String, CompilerType] = {
    case "interpreter" => INTERPRETER
    case "default" => DEFAULT_COMPILER
    case "tac" => TAC
    case "vm_compiler" => VM_COMPILER
    case _ => sys.error("Compiler type unsupported.")
  }
}

class Flags {
  var compilerType: CompilerType = DEFAULT_COMPILER
}
