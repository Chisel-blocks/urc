// Finitie impulse filter
package urc
import config._
import config.{UrcConfig}
import f2_interpolator.config.{F2Config => intF2Config}
import f2_decimator.config.{F2Config => decF2Config}
import cic_interpolator.config.{CicConfig => intCicConfig}
import cic_decimator.config.{CicConfig => decCicConfig}
import hb_interpolator.config.{HbConfig => intHbConfig}
import hb_decimator.config.{HbConfig => decHbConfig}

import java.io.File

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.stage.ChiselGeneratorAnnotation

import dsptools._
import dsptools.numbers.DspComplex

import f2_interpolator._
import f2_decimator._

class URCCLK extends Bundle {
    val f2intclock_high = Input(Clock())
    val f2decclock_low = Input(Clock())
}

class URCIO(resolution: Int, gainBits: Int) extends Bundle {
    val clock = new URCCLK  
    val in = new Bundle {
        val iptr_A = Input(DspComplex(SInt(resolution.W), SInt(resolution.W)))
    }
    val out = new Bundle {
        val Z = Output(DspComplex(SInt(resolution.W), SInt(resolution.W)))
    }
}

class URC(config: UrcConfig) extends Module {
    val io = IO(new URCIO(resolution=config.resolution, gainBits=config.gainBits))
    val data_reso = config.resolution
    val calc_reso = config.resolution * 2

   //Reset initializations
    val f2intreset = Wire(Bool())
    val f2decreset = Wire(Bool())
    f2intreset := reset.asBool
    f2decreset := reset.asBool

    val f2int = withClockAndReset(io.clock.f2intclock_high, f2intreset)(Module( 
        new F2_Interpolator(config=config.f2int_config)
    ))

    val f2dec = withClockAndReset(io.clock.f2decclock_low, f2decreset)(Module( 
        new F2_Decimator(config=config.f2dec_config)
    ))
}



/** Generates verilog or sv*/
object URC extends App with OptionParser {
    // Parse command-line arguments
    val (options, arguments) = getopts(default_opts, args.toList)
    printopts(options, arguments)

    val urc_config_file = options("urc_config_file")
    val intf2_config_file = options("intf2_config_file")
    val inthb1_config_file = options("inthb1_config_file")
    val inthb2_config_file = options("inthb2_config_file")
    val inthb3_config_file = options("inthb3_config_file")
    val intcic3_config_file = options("intcic3_config_file")
    val decf2_config_file = options("decf2_config_file")
    val dechb1_config_file = options("dechb1_config_file")
    val dechb2_config_file = options("dechb2_config_file")
    val dechb3_config_file = options("dechb3_config_file")
    val deccic3_config_file = options("deccic3_config_file")
    val target_dir = options("td")
    
    var urc_config: Option[UrcConfig] = None

    //Load interpolator
    var intf2_config: Option[intF2Config] = None
    var inthb1_config: Option[intHbConfig] = None
    var inthb2_config: Option[intHbConfig] = None
    var inthb3_config: Option[intHbConfig] = None
    var intcic3_config: Option[intCicConfig] = None

    intHbConfig.loadFromFile(inthb1config_file) match {
        case Left(config) => {
            inthb1_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 int hb1 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    intHbConfig.loadFromFile(inthb2config_file) match {
        case Left(config) => {
            inthb2_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 int hb2 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    intHbConfig.loadFromFile(inthb3config_file) match {
        case Left(config) => {
            hb3int_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 int hb3 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    intCicConfig.loadFromFile(intcic3config_file) match {
        case Left(config) => {
            intcic3_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 int cic3 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    intF2Config.loadFromFile(intf2_config_file, inthb1_config.get, inthb2_config.get, inthb3_config.get, intcic3_config.get) match {
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
    var dechb1_config: Option[decHbConfig] = None
    var dechb2_config: Option[decHbConfig] = None
    var dechb3_config: Option[decHbConfig] = None
    var deccic3_config: Option[decCicConfig] = None

    decHbConfig.loadFromFile(dechb1config_file) match {
        case Left(config) => {
            dechb1int_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 dec hb1 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    decHbConfig.loadFromFile(dechb2config_file) match {
        case Left(config) => {
            dechb2int_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 dec hb2 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    decHbConfig.loadFromFile(dechb3config_file) match {
        case Left(config) => {
            dechb3int_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 dec hb3 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    decCicConfig.loadFromFile(deccic3config_file) match {
        case Left(config) => {
            deccic3int_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 dec cic3 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    decF2Config.loadFromFile(decf2_config_file, dechb1_config.get, dechb2_config.get, dechb3_config.get, deccic3_config.get) match {
        case Left(config) => {
            decf2int_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 dec configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    URCConfig.loadFromFile(urc_config_file, intf2_config.get, decf2_config.get) match {
        case Left(config) => {
            urc_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load URC configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    // Generate verilog
    val annos = Seq(ChiselGeneratorAnnotation(() => new URC(config=urc_config.get)))
    val sysverilog = (new ChiselStage).emitSystemVerilog(
        new URC(config=urc_config.get),
     
    //args
    Array("--target-dir", target_dir))
}



/** Module-specific command-line option parser */
trait OptionParser {
  // Module specific command-line option flags
  val available_opts: List[String] = List(
      "-urc_config_file",
      "-intf2_config_file",
      "-inthb1_config_file",
      "-inthb2_config_file",
      "-inthb3_config_file",
      "-intcic3_config_file",
      "-decf2_config_file",
      "-dechb1_config_file",
      "-dechb2_config_file",
      "-dechb3_config_file",
      "-deccic3_config_file",
      "-td"
  )

  // Default values for the command-line options
  val default_opts : Map[String, String] = Map(
    "urc_config_file"->"urc-config.yml",
    "intf2_config_file"->"f2_interpolator/configs/f2-config.yml",
    "inthb1_config_file"->"f2_interpolator/hb_interpolator/configs/hb1-config.yml",
    "inthb2_config_file"->"f2_interpolator/hb_interpolator/configs/hb2-config.yml",
    "inthb3_config_file"->"f2_interpolator/hb_interpolator/configs/hb3-config.yml",
    "intcic3_config_file"->"f2_interpolator/cic_interpolator/configs/cic3-config.yml",
    "decf2_config_file"->"f2_decimator/configs/f2-config.yml",
    "dechb1_config_file"->"f2_decimator/hb_decimator/configs/hb1-config.yml",
    "dechb2_config_file"->"f2_decimator/hb_decimator/configs/hb2-config.yml",
    "dechb3_config_file"->"f2_decimator/hb_decimator/configs/hb3-config.yml",
    "deccic3_config_file"->"f2_decimator/cic3_decimator/configs/cic3-config.yml",
    "td"->"verilog/"
  )

  /** Recursively parse option flags from command line args
   * @param options Map of command line option names to their respective values.
   * @param arguments List of arguments to parse.
   * @return a tuple whose first element is the map of parsed options to their values 
   *         and the second element is the list of arguments that don't take any values.
   */
  def getopts(options: Map[String, String], arguments: List[String]) : (Map[String, String], List[String]) = {
    val usage = s"""
      |Usage: ${this.getClass.getName.replace("$","")} [-<option> <argument>]
      |
      | Options
      |     -urc_config_file       [String]  : Generator YAML configuration file name. Default "urc-config.yml".
      |     -intf2_config_file     [String]  : Generator YAML configuration file name. Default "f2-config.yml".
      |     -inthb1_config_file    [String]  : Generator YAML configuration file name. Default "hb1-config.yml".
      |     -inthb2_config_file    [String]  : Generator YAML configuration file name. Default "hb2-config.yml".
      |     -inthb3_config_file    [String]  : Generator YAML configuration file name. Default "hb3-config.yml".
      |     -intcic3_config_file   [String]  : Generator YAML configuration file name. Default "cic3-config.yml".
      |     -decf2_config_file     [String]  : Generator YAML configuration file name. Default "f2-config.yml".
      |     -dechb1_config_file    [String]  : Generator YAML configuration file name. Default "hb1-config.yml".
      |     -dechb2_config_file    [String]  : Generator YAML configuration file name. Default "hb2-config.yml".
      |     -dechb3_config_file    [String]  : Generator YAML configuration file name. Default "hb3-config.yml".
      |     -deccic3_config_file   [String]  : Generator YAML configuration file name. Default "cic3-config.yml".
      |     -td                    [String]  : Target dir for building. Default "verilog/".
      |     -h                               : Show this help message.
      """.stripMargin

    // Parse next elements in argument list
    arguments match {
      case "-h" :: tail => {
        println(usage)
        sys.exit()
      }
      case option :: value :: tail if available_opts contains option => {
        val (newopts, newargs) = getopts(
            options ++ Map(option.replace("-","") -> value), tail
        )
        (newopts, newargs)
      }
      case argument :: tail => {
        val (newopts, newargs) = getopts(options, tail)
        (newopts, argument.toString +: newargs)
      }
      case Nil => (options, arguments)
    }
  }

  /** Print parsed options and arguments to stdout */
  def printopts(options: Map[String, String], arguments: List[String]) = {
    println("\nCommand line options:")
    options.nonEmpty match {
      case true => for ((k,v) <- options) {
        println(s"  $k = $v")
      }
      case _ => println("  None")
    }
    println("\nCommand line arguments:")
    arguments.nonEmpty match {
      case true => for (arg <- arguments) {
        println(s"  $arg")
      }
      case _ => println("  None")
    }
  }
}

