// See LICENSE_AALTO.txt for license details

package urc.config

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.math.BigInt
import scala.io.Source
import chisel3._

import f2_universal.config.{f2Config}

case class URCGeneric(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int
)

case class URCConfig(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int,
  f2_config:          f2Config,
)

object URCConfig {
  implicit val URCGenericFormat = yamlFormat3(URCGeneric)

  // TODO: Update this to always match the major version number of the release
  val syntaxVersion = 2

  /** Exception type for FIR config parsing errors */
  class URCConfigParseException(msg: String) extends Exception(msg)

  /** Type for representing error return values from a function */
  case class Error(msg: String) {
    /** Throw a parsing exception with a debug message. */
    def except() = { throw new URCConfigParseException(msg) }

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

  def loadFromFile(
    urc_file: String = "urc-config.yml", 
    f2_file: String = "urc/f2_universal/configs/f2-config.yml", 
    hb1_file: String = "urc/f2_universal/hb_universal/configs/hb1-config.yml", 
    hb2_file: String = "urc/f2_universal/hb_universal/configs/hb2-config.yml", 
    hb3_file: String = "urc/f2_universal/hb_universal/configs/hb3-config.yml", 
    cic3_file: String = "urc/f2_universal/cic_universal/configs/cic3-config.yml",
    ): Either[URCConfig, Error] = {

    println(s"\nLoading URC configuration from file: $urc_file")
    var URC_fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(urc_file)
      URC_fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }
    
    // print file contents as troubleshooting info
    println("\nYAML configuration file contents:")
    //println(s"```\n$fileString\n```")

    // Determine syntax version
    val URCyamlAst = URC_fileString.parseYaml

    val syntaxVersion = parseSyntaxVersion(URCyamlAst)
    syntaxVersion match {
      case Left(value) => ()
      case Right(err) => return Right(err)
    }

    // Parse FirConfig from YAML AST
    val urc_generic = URCyamlAst.convertTo[URCGeneric]

   //Load interpolator
    var f2_config: Option[f2Config] = None

    f2Config.loadFromFile(
        f2_file, 
        hb1_file, 
        hb2_file, 
        hb3_file, 
        cic3_file) match {
        case Left(config) => {
            f2_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load f2 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    val config = new URCConfig(
	    urc_generic.syntax_version, 
	    urc_generic.resolution, 
	    urc_generic.gainBits,
        f2_config.get
    )

    println("resolution:")
    println(config.resolution)

    println("gainBits:")
    println(config.gainBits)

    Left(config)
  }
}
