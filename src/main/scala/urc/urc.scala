// Finitie impulse filter
package urc
import config._
import config.{urcConfig}

import java.io.File

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

import dsptools._
import dsptools.numbers.DspComplex

import f2_universal._
import clkdiv_n_2_4_8._

class urcCTRL(val resolution : Int, val gainBits: Int) extends Bundle {
    val cic3scale = Input(UInt(gainBits.W))
    val reset_loop = Input(Bool())
    val ndiv = Input(UInt(8.W))
    val reset_clock = Input(Bool())
    val hb1scale = Input(UInt(gainBits.W))
    val hb1output_switch = Input(UInt(1.W))
    val hb2scale = Input(UInt(gainBits.W))
    val hb2output_switch = Input(UInt(1.W))
    val hb3scale = Input(UInt(gainBits.W))
    val hb3output_switch = Input(UInt(1.W))
    val mode = Input(UInt(3.W))
    val convmode = Input(UInt(1.W))
}

class urcIO(resolution: Int, gainBits: Int) extends Bundle {
    val control = new urcCTRL(resolution=resolution, gainBits=gainBits)
    val in = new Bundle {
        val iptr_A = Input(DspComplex(SInt(resolution.W), SInt(resolution.W)))
    }
    val out = new Bundle {
        val Z = Output(DspComplex(SInt(resolution.W), SInt(resolution.W)))
    }
}

class urc(config: urcConfig) extends Module {
    val io = IO(new urcIO(resolution=config.resolution, gainBits=config.gainBits))
    val data_reso = config.resolution
    val calc_reso = config.resolution * 2

    val czero  = DspComplex(0.S(data_reso.W),0.S(data_reso.W)) //Constant complex zero

   //Reset initializations
    val clkreset = Wire(Bool())
    val f2reset = Wire(Bool())

    clkreset := io.control.reset_clock
    f2reset := reset.asBool

    val clkdiv = withReset(reset)(Module( 
        new clkdiv_n_2_4_8(n=8)
    ))

    clkdiv.io.reset_clk := io.control.reset_clock
    clkdiv.io.Ndiv := io.control.ndiv
    val f2_mclock = Wire(Bool())
    f2_mclock := clock.asBool

    val f2 = withClockAndReset(f2_mclock.asClock, f2reset)(Module( 
        new f2_universal(config=config.f2_config)
    ))

    f2.io.in.iptr_A := io.in.iptr_A
    io.out.Z        := f2.io.out.Z

    f2.io.control.cic3scale          := io.control.cic3scale

    f2.io.control.hb1scale          := io.control.hb1scale
    f2.io.control.hb1output_switch  := io.control.hb1output_switch

    f2.io.control.hb2scale          := io.control.hb2scale
    f2.io.control.hb2output_switch  := io.control.hb2output_switch

    f2.io.control.hb3scale          := io.control.hb3scale
    f2.io.control.hb3output_switch  := io.control.hb3output_switch

    f2.io.control.mode          := io.control.mode
    f2.io.control.convmode      := io.control.convmode

    f2.io.control.reset_loop    := io.control.reset_loop
    f2.io.control.reset_clk     := io.control.reset_clock

    f2.io.clock.p8n   := Mux(f2reset.asBool, clock.asUInt.asBool, clkdiv.io.clkp8n).asClock
    f2.io.clock.p4n   := Mux(f2reset.asBool, clock.asUInt.asBool, clkdiv.io.clkp4n).asClock
    f2.io.clock.p2n   := Mux(f2reset.asBool, clock.asUInt.asBool, clkdiv.io.clkp2n).asClock
    f2.io.clock.pn    := Mux(f2reset.asBool, clock.asUInt.asBool, clkdiv.io.clkpn).asClock

    //Modes
    when(io.control.mode === 1.U){ // Two
        clkdiv.io.shift := 3.U(3.W)
        f2_mclock := clkdiv.io.clkp4n
    }.elsewhen(io.control.mode === 2.U){ //Four
        clkdiv.io.shift := 2.U(3.W)
        f2_mclock := clkdiv.io.clkp2n
    }.elsewhen(io.control.mode === 3.U){ //Eight
        clkdiv.io.shift := 1.U(3.W)
        f2_mclock := clkdiv.io.clkpn
    }.elsewhen(io.control.mode === 4.U){ //More
        clkdiv.io.shift := 0.U(3.W)
        f2_mclock := clock.asBool
    }.otherwise{ //Bypass
        clkdiv.io.shift := 4.U(3.W)
        f2_mclock := clkdiv.io.clkp8n
    }
}



/** Generates verilog or sv*/
object urc extends App with OptionParser {
    // Parse command-line arguments
    val (options, arguments) = getopts(default_opts, args.toList)
    printopts(options, arguments)

    val urc_config_file = options("urc_config_file")
    val f2_config_file = options("f2_config_file")
    val hb1_config_file = options("hb1_config_file")
    val hb2_config_file = options("hb2_config_file")
    val hb3_config_file = options("hb3_config_file")
    val cic3_config_file = options("cic3_config_file")
    val target_dir = options("td")
    
    var urc_config: Option[urcConfig] = None

    urcConfig.loadFromFile(
        urc_config_file, 
        f2_config_file,
        hb1_config_file,
        hb2_config_file,
        hb3_config_file,
        cic3_config_file) match {
        case Left(config) => {
            urc_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load URC configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    // Generate verilog
    val annos = Seq(ChiselGeneratorAnnotation(() => new urc(config=urc_config.get)))
    val verilog = (new ChiselStage).emitVerilog(
        new urc(config=urc_config.get),
     
    //args
    Array("--target-dir", target_dir))
}



/** Module-specific command-line option parser */
trait OptionParser {
  // Module specific command-line option flags
  val available_opts: List[String] = List(
      "-urc_config_file",
      "-f2_config_file",
      "-hb1_config_file",
      "-hb2_config_file",
      "-hb3_config_file",
      "-cic3_config_file",
      "-td"
  )

  // Default values for the command-line options
  val default_opts : Map[String, String] = Map(
    "urc_config_file"->"configs/urc-config.yml",
    "f2_config_file"->"f2_universal/configs/f2-config.yml",
    "hb1_config_file"->"f2_universal/hb_universal/configs/hb1-config.yml",
    "hb2_config_file"->"f2_universal/hb_universal/configs/hb2-config.yml",
    "hb3_config_file"->"f2_universal/hb_universal/configs/hb3-config.yml",
    "cic3_config_file"->"f2_universal/cic_universal/configs/cic3-config.yml",
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
      |     -f2_config_file        [String]  : Generator YAML configuration file name. Default "f2-config.yml".
      |     -hb1_config_file       [String]  : Generator YAML configuration file name. Default "hb1-config.yml".
      |     -hb2_config_file       [String]  : Generator YAML configuration file name. Default "hb2-config.yml".
      |     -hb3_config_file       [String]  : Generator YAML configuration file name. Default "hb3-config.yml".
      |     -cic3_config_file      [String]  : Generator YAML configuration file name. Default "cic3-config.yml".
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

