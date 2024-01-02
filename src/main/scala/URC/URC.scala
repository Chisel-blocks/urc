// Finitie impulse filter
package urc
import config._
import config.{UrcConfig}

import java.io.File

import chisel3._
import chisel3.util._
import chisel3.util.{log2Ceil}
import chisel3.experimental.FixedPoint
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.stage.ChiselGeneratorAnnotation

import dsptools._
import dsptools.numbers.DspComplex

import f2_interpolator._
import f2_decimator._
import clkdiv_n_2_4_8._

object UrcStates {
    object State extends ChiselEnum {
        val bypass, two, four, eight, more = Value
    }  
}

class URCCTRL(val resolution : Int, val gainBits: Int) extends Bundle {
    val cic3scale = Input(UInt(gainBits.W))
    val cic3shift = Input(UInt(log2Ceil(resolution).W))
    val reset_loop = Input(Bool())
    val hb1scale = Input(UInt(gainBits.W))
    val hb2scale = Input(UInt(gainBits.W))
    val hb3scale = Input(UInt(gainBits.W))
    val mode = Input(UInt(3.W))
    val convmode = Input(UInt(1.W))
}

class URCIO(resolution: Int, gainBits: Int) extends Bundle {
    val control = new URCCTRL(resolution=resolution, gainBits=gainBits)
    val in = new Bundle {
        val iptr_A = Input(UInt((resolution * 2).W))
    }
    val out = new Bundle {
        val Z = Output(UInt((resolution * 2).W))
    }
}

class URC(config: UrcConfig) extends Module {
    val io = IO(new URCIO(resolution=config.resolution, gainBits=config.gainBits))
    val data_reso = config.resolution
    val calc_reso = config.resolution * 2

    val czero  = DspComplex(0.S(data_reso.W),0.S(data_reso.W)) //Constant complex zero

    val iptr_A_IQ = RegInit(DspComplex(0.S(data_reso.W),0.S(data_reso.W)))
    iptr_A_IQ.real := io.in.iptr_A(31,15).asSInt
    iptr_A_IQ.imag := io.in.iptr_A(15,0).asSInt

    val Z_IQ = RegInit(DspComplex(0.S(data_reso.W),0.S(data_reso.W)))
    io.out.Z := Cat(Z_IQ.real.asUInt, Z_IQ.imag.asUInt)

    import UrcStates.State
    import UrcStates.State._

    //Select state with master clock
    val state = RegInit(bypass)

    //Decoder for the modes
    when(io.control.mode === 0.U){
        state := bypass
    } .elsewhen(io.control.mode === 1.U) {
        state := two
    } .elsewhen(io.control.mode === 2.U) {
        state := four
    } .elsewhen(io.control.mode === 3.U) {
        state := eight
    } .elsewhen(io.control.mode === 4.U) {
        state := more
    }.otherwise {
        state := bypass
    }

   //Reset initializations
    val clkreset = Wire(Bool())
    val f2intreset = Wire(Bool())
    val f2decreset = Wire(Bool())

    clkreset := reset.asBool
    f2intreset := reset.asBool
    f2decreset := reset.asBool

    val clkdiv = withClockAndReset(clock, clkreset)(Module( 
        new clkdiv_n_2_4_8(n=8)
    ))

    val f2int = withClockAndReset(clock, f2intreset)(Module( 
        new F2_Interpolator(config=config.f2int_config)
    ))

    f2int.io.control.cic3derivscale := io.control.cic3scale
    f2int.io.control.cic3derivshift := io.control.cic3shift
    f2int.io.control.reset_loop     := io.control.reset_loop
    f2int.io.control.hb1scale       := io.control.hb1scale
    f2int.io.control.hb2scale       := io.control.hb2scale
    f2int.io.control.hb3scale       := io.control.hb3scale
    f2int.io.control.mode           := io.control.mode


    f2int.io.clock.hb1clock_low := clkdiv.io.clkp8n.asClock
    f2int.io.clock.hb1clock_high := clkdiv.io.clkp4n.asClock
    f2int.io.clock.hb2clock_high := clkdiv.io.clkp2n.asClock
    f2int.io.clock.hb3clock_high := clkdiv.io.clkpn.asClock
    f2int.io.clock.cic3clockfast := clock

    val f2dec = withClockAndReset(clock, f2decreset)(Module( 
        new F2_Decimator(config=config.f2dec_config)
    ))    

    f2dec.io.control.cic3integscale := io.control.cic3scale
    f2dec.io.control.cic3integshift := io.control.cic3shift
    f2dec.io.control.reset_loop     := io.control.reset_loop
    f2dec.io.control.hb1scale       := io.control.hb1scale
    f2dec.io.control.hb2scale       := io.control.hb2scale
    f2dec.io.control.hb3scale       := io.control.hb3scale
    f2dec.io.control.mode           := io.control.mode

    f2dec.io.clock.cic3clockslow := clkdiv.io.clkpn.asClock
    f2dec.io.clock.hb1clock_low  := clkdiv.io.clkp2n.asClock
    f2dec.io.clock.hb2clock_low  := clkdiv.io.clkp4n.asClock
    f2dec.io.clock.hb3clock_low  := clkdiv.io.clkp8n.asClock

    when(io.control.convmode.asBool){
        f2intreset := true.B
        f2int.io.in.iptr_A := czero

        f2dec.io.in.iptr_A          := iptr_A_IQ
        f2decreset                  := reset.asBool
        f2dec.io.control.reset_loop := io.control.reset_loop
        Z_IQ                        := f2dec.io.out.Z
    } .otherwise {
        f2decreset := true.B
        f2dec.io.in.iptr_A := czero

        f2int.io.in.iptr_A           := iptr_A_IQ
        f2intreset                   := reset.asBool
        f2int.io.control.reset_loop  := io.control.reset_loop
        Z_IQ                         := f2int.io.out.Z
    }

    clkdiv.io.reset_clk := clkreset
    clkdiv.io.Ndiv := 2.U
    clkdiv.io.shift := 0.U

    //Modes
    when(state === two){
        clkdiv.io.shift := 3.U
    }.elsewhen(state === four){
        clkdiv.io.shift := 2.U
    }.elsewhen(state === eight){
        clkdiv.io.shift := 1.U
    }.elsewhen(state === more){
        clkdiv.io.shift := 0.U
    }.otherwise{
        // Bypass
        clkdiv.io.shift := 4.U
    }
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

    UrcConfig.loadFromFile(
        urc_config_file, 
        intf2_config_file,
        inthb1_config_file,
        inthb2_config_file,
        inthb3_config_file,
        intcic3_config_file,
        decf2_config_file,
        dechb1_config_file,
        dechb2_config_file,
        dechb3_config_file,
        deccic3_config_file) match {
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
    "deccic3_config_file"->"f2_decimator/cic_decimator/configs/cic3-config.yml",
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

