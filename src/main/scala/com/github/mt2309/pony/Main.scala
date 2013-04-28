package com.github.mt2309.pony

import java.io.IOException

/**
 * User: mthorpe
 * Date: 25/04/2013
 * Time: 22:44
 */

case class Config(stage: Int = 3, help: Boolean = false, version: Boolean = false, input: String = ".", output: String = "a.out")

object Main extends App {

  val stages: String = "\t\t(Stage 1: parser\n\t\tStage 2: typer\n\t\tStage 3: code-gen)"

  val parser = new scopt.immutable.OptionParser[Config]("ponyC", "0.1-ALPHA") {
    override def options = Seq(
      intOpt("s", "stage", "what stage of the compiler to run to (mostly used for debugging)\n" + stages) {(v: Int, c: Config) => c.copy(stage = v)},
      booleanOpt("v", "version", "Version") {(b: Boolean, c: Config) => c.copy(version = b)},
      booleanOpt("h", "help", "print usage message") {(b: Boolean, c: Config) => c.copy(version = b)},
      opt("i", "input", "input directory") {(i: String, c:Config) => c.copy(input = i)},
      opt("o", "output", "output file") {(o: String, c:Config) => c.copy(output =  o)}
    )
  }

  parser.parse(args, Config()) map {
    config => {
      if (config.version) version()
      else if (config.help) help()
      else compile(config.input, config.output, config.stage)
    }
  } getOrElse parser.showUsage

  def version() {
    println(parser.version.get)
  }

  def help() {
    parser.showUsage
  }

  def compile(input: String, output: String, stage: Int) {
    val unit = try {
      Loader.Loader.load(stage, input)
    } catch {
      case e: IOException => {println("Starting directory " + input + " not found"); sys.exit(1)}
    }

    println("here")
    unit.buildUnit
  }
}
