package com.grok

import java.io.File

/**
 * Created by brendan on 3/4/15.
 */
object Main {
  def main(args: Array[String]): Unit = {
    val (flags, sources) = parseFlags(args)
    sources.foreach(validateSource)

    val compiler = CompilerFactory.compiler(flags)
    compiler.compile(sources)
  }

  def parseFlags(args: Seq[String]): (Flags, Seq[String]) = {
    val flags = new Flags
    val parseFlag: PartialFunction[(String, String), Boolean] = {
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
    case "default" => DEFAULT_COMPILER
  }
}

class Flags {
  var compilerType = DEFAULT_COMPILER
}
