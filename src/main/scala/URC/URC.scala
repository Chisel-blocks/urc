// Finitie impulse filter
package urc
import config._
import config.{UrcConfig}

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
    val f2_intclock_high = Input(Clock())
    val f2_decclock_low = Input(Clock())
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
    val f2int_config_file = options("f2int_config_file")
    val f2dec_config_file = options("f2dec_config_file")
    val target_dir = options("td")
    var urc_config: Option[F2Config] = None

    UrcConfig.loadFromFiles(urc_config_file, f2int_config_file, f2dec_config_file) match {
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
      "-f2int_config_file",
      "-f2dec_config_file",
      "-td"
  )

  // Default values for the command-line options
  val default_opts : Map[String, String] = Map(
    "urc_config_file"->"urc-config.yml",
    "f2int_config_file"->"f2_interpolator/configs/f2int-config.yml",
    "f2dec_config_file"->"f2_decimator/configs/f2dec-config.yml",
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
      |     -f2int_config_file     [String]  : Generator YAML configuration file name. Default "f2int-config.yml".
      |     -f2dec_config_file     [String]  : Generator YAML configuration file name. Default "f2dec-config.yml".
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

