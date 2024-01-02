// See LICENSE_AALTO.txt for license details

package urc.config

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.math.BigInt
import scala.io.Source
import chisel3._

import f2_interpolator.config.{F2Config => intF2Config}
import f2_decimator.config.{F2Config => decF2Config}

case class UrcGeneric(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int
)

case class UrcConfig(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int,
  f2int_config:       intF2Config,
  f2dec_config:       decF2Config,
)

object UrcConfig {
  implicit val UrcGenericFormat = yamlFormat3(UrcGeneric)

  // TODO: Update this to always match the major version number of the release
  val syntaxVersion = 2

  /** Exception type for FIR config parsing errors */
  class UrcConfigParseException(msg: String) extends Exception(msg)

  /** Type for representing error return values from a function */
  case class Error(msg: String) {
    /** Throw a parsing exception with a debug message. */
    def except() = { throw new UrcConfigParseException(msg) }

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
    urc_file: String, 
    intf2_file: String, 
    inthb1_file: String, 
    inthb2_file: String, 
    inthb3_file: String, 
    intcic3_file: String,
    decf2_file: String, 
    dechb1_file: String, 
    dechb2_file: String, 
    dechb3_file: String, 
    deccic3_file: String
    ): Either[UrcConfig, Error] = {

    println(s"\nLoading Urc configuration from file: $urc_file")
    var Urc_fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(urc_file)
      Urc_fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }
    
    // print file contents as troubleshooting info
    println("\nYAML configuration file contents:")
    //println(s"```\n$fileString\n```")

    // Determine syntax version
    val UrcyamlAst = Urc_fileString.parseYaml

    val syntaxVersion = parseSyntaxVersion(UrcyamlAst)
    syntaxVersion match {
      case Left(value) => ()
      case Right(err) => return Right(err)
    }

    // Parse FirConfig from YAML AST
    val urc_generic = UrcyamlAst.convertTo[UrcGeneric]

   //Load interpolator
    var intf2_config: Option[intF2Config] = None

    intF2Config.loadFromFile(
        intf2_file, 
        inthb1_file, 
        inthb2_file, 
        inthb3_file, 
        intcic3_file) match {
        case Left(config) => {
            intf2_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 int configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    //Load decimator
    var decf2_config: Option[decF2Config] = None

    decF2Config.loadFromFile(
        decf2_file, 
        dechb1_file, 
        dechb2_file, 
        dechb3_file, 
        deccic3_file) match {
        case Left(config) => {
            decf2_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 dec configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    val config = new UrcConfig(
	    urc_generic.syntax_version, 
	    urc_generic.resolution, 
	    urc_generic.gainBits,
        intf2_config.get,
        decf2_config.get
    )

    println("resolution:")
    println(config.resolution)

    println("gainBits:")
    println(config.gainBits)

    Left(config)
  }
}
