// See LICENSE_AALTO.txt for license details

package urc.config

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.math.BigInt
import scala.io.Source
import chisel3._

import f2_interpolator.config.{F2Config}

case class UrcGeneric(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int
}

case class UrcConfig(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int,
  f2int_config:       F2Config,
  f2dec_config:       F2Config,
)

object F2Config {
  implicit val UrcGenericFormat = yamlFormat3(UrcGeneric)
  implicit val F2ConfigFormat = yamlFormat4(F2Config.apply)

  // TODO: Update this to always match the major version number of the release
  val syntaxVersion = 2

  /** Exception type for FIR config parsing errors */
  class UrcConfigParseException(msg: String) extends Exception(msg)

  /** Type for representing error return values from a function */
  case class Error(msg: String) {
    /** Throw a parsing exception with a debug message. */
    def except() = { throw new F2ConfigParseException(msg) }

    /** Abort program execution and print out the reason */
    def panic() = {
      System.err.println(msg)
      System.exit(-1)
    }
  }

  /** parse legal syntax version from config yaml AST */
  private[config] def parseSyntaxVersion(yamlAst: YamlValue): Either[BigInt,Error] = {
    // get version number as an integer
    val version: BigInt = yamlAst.asYamlObject.fields.get(YamlString("syntax_version")) match {
      case Some(version) => version match {
        case maybeDecimal: YamlNumber => maybeDecimal.asInstanceOf[YamlNumber].value.toBigIntExact match {
          case Some(integer) => integer
          case None => return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
        }
        case _ => return return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
      }
      case None => return Right(Error("Missing required top-level key: `syntax_version`."))
    }
    if (syntaxVersion != version)
      return Right(Error(s"Unsupported syntax version: $version.\n- Supported versions: $syntaxVersion"))
    Left(version)
  }

  def loadFromFiles(urcfile: String, f2intfile: String, f2decfile: String): Either[UrcConfig, Error] = {
    println(s"\nLoading Urc configuration from file: $urcfile")
    var UrcfileString: String = ""
    try {
      val bufferedSource = Source.fromFile(urcfile)
      UrcfileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }

    println(s"\nLoading F2int configuration from file: $f2intfile")
    var F2intfileString: String = ""
    try {
      val bufferedSource = Source.fromFile(f2intfile)
      F2intfileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }

    println(s"\nLoading F2dec configuration from file: $f2decfile")
    var F2decfileString: String = ""
    try {
      val bufferedSource = Source.fromFile(f2decfile)
      F2decfileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }
    
    // print file contents as troubleshooting info
    println("\nYAML configuration file contents:")
    //println(s"```\n$fileString\n```")

    // Determine syntax version
    val UrcyamlAst = UrcfileString.parseYaml
    val F2intyamlAst = F2intfileString.parseYaml
    val F2decyamlAst = F2decfileString.parseYaml

    val syntaxVersion = parseSyntaxVersion(UrcyamlAst)
    syntaxVersion match {
      case Left(value) => ()
      case Right(err) => return Right(err)
    }

    // Parse FirConfig from YAML AST
    val urc_generic = UrcyamlAst.convertTo[UrcGeneric]
    val f2int_config = F2intyamlAst.convertTo[F2Config]
    val f2dec_config = F2decyamlAst.convertTo[F2Config]

    val config = new UrcConfig(
	    urc_generic.syntax_version, 
	    urc_generic.resolution, 
	    urc_generic.gainBits,
        f2int_config,
        f2dec_config
    )

    println("resolution:")
    println(config.resolution)

    println("gainBits:")
    println(config.gainBits)

    println("hb1:")
    println(config.f2int_config)

    println("hb2:")
    println(config.f2dec_config)

    Left(config)
  }
}
