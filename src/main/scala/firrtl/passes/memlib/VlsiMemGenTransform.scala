package firrtl.passes.memlib

import firrtl.Utils.error
import firrtl.annotations.NoTargetAnnotation
import firrtl.{bitWidth, CircuitState, DependencyAPIMigration, Transform}
import firrtl.stage.Forms

case class GenVerilogMemBehaviorModelAnno(annotatedMemories: Seq[DefAnnotatedMemory]) extends NoTargetAnnotation

class VlsiMemGenTransform extends Transform with DependencyAPIMigration {
  override def prerequisites = Forms.MidForm
  override protected def execute(state: CircuitState): CircuitState = {

    // No `MaskedWritePort`, just `WritePort` with `masked` == true
    case class Port(prefix: String, `type`: MemPort, masked: Boolean)

    def parse(annotatedMemory: DefAnnotatedMemory): (String, Int, Int, Int, Int, Seq[Port]) = {
      val masked = annotatedMemory.maskGran.isDefined

      val name = annotatedMemory.name
      val width = {
        val width = bitWidth(annotatedMemory.dataType)
        require(width <= Int.MaxValue)
        width.toInt
      }
      val depth = annotatedMemory.depth.toInt
      val maskGran = if (masked) {
        val maskGran = annotatedMemory.maskGran.get
        require(maskGran <= Int.MaxValue)
        maskGran.toInt
      } else width
      val ports = annotatedMemory.readers.indices.map(number => Port(s"R${number}_", ReadPort, masked)) ++
        annotatedMemory.writers.indices.map(number => Port(s"W${number}_", WritePort, masked)) ++
        annotatedMemory.readwriters.indices.map(number => Port(s"RW${number}_", ReadWritePort, masked))

      (name, width, depth, maskGran, width / maskGran, ports)
    }

    def genMem(name: String, width: Int, depth: Int, maskGran: Int, maskSeg: Int, ports: Seq[Port]): String = {
      val addrWidth = math.max(math.ceil(math.log(depth) / math.log(2)).toInt, 1)
      val readPorts = ports.filter(port => port.`type` == ReadPort || port.`type` == ReadWritePort)

      def genPortSpec(port: Port): Seq[String] = {
        Seq(s"input ${port.prefix}clk", s"input [${addrWidth - 1}:0] ${port.prefix}addr", s"input ${port.prefix}en") ++
          (port match {
            case Port(prefix, ReadPort, _)      => Seq(s"output [${width - 1}:0] ${prefix}data")
            case Port(prefix, WritePort, false) => Seq(s"input [${width - 1}:0] ${prefix}data")
            case Port(prefix, WritePort, true) =>
              Seq(s"input [${width - 1}:0] ${prefix}data", s"input [${maskSeg - 1}:0] ${prefix}mask")
            case Port(prefix, ReadWritePort, false) =>
              Seq(
                s"input ${prefix}wmode",
                s"input [${width - 1}:0] ${prefix}wdata",
                s"output [${width - 1}:0] ${prefix}rdata"
              )
            case Port(prefix, ReadWritePort, true) =>
              Seq(
                s"input ${prefix}wmode",
                s"input [${maskSeg - 1}:0] ${prefix}wmask",
                s"input [${width - 1}:0] ${prefix}wdata",
                s"output [${width - 1}:0] ${prefix}rdata"
              )
          })
      }
      val portSpec = ports.flatMap(genPortSpec)

      def genDecl(): Seq[String] = readPorts.flatMap(port =>
        Seq(s"reg reg_${port.prefix}ren;", s"reg [${addrWidth - 1}:0] reg_${port.prefix}addr;")
      ) ++ Seq(
        s"reg [${width - 1}:0] ram [${depth - 1}:0];",
        "`ifdef RANDOMIZE_MEM_INIT",
        "  integer initvar;",
        "  initial begin",
        "    #`RANDOMIZE_DELAY begin end",
        s"    for (initvar = 0; initvar < $depth; initvar = initvar+1)",
        s"      ram[initvar] = {${(width - 1) / 32 + 1} {$$random}};"
      ) ++ readPorts.map(port => s"    reg_${port.prefix}addr = {${(addrWidth - 1) / 32 + 1} {$$random}};") ++ Seq(
        "  end",
        "`endif"
      )
      val decl = genDecl()

      def genSequential(port: Port): Seq[String] = {
        def genReadSequential(en: String) = Seq(
          s"always @(posedge ${port.prefix}clk)",
          s"  reg_${port.prefix}ren <= $en;",
          s"always @(posedge ${port.prefix}clk)",
          s"  if ($en) reg_${port.prefix}addr <= ${port.prefix}addr;"
        )
        def genWriteSequential(en: String, inputData: String): Seq[String] = Seq(
          s"always @(posedge ${port.prefix}clk)",
          s"  if ($en) begin"
        ) ++ (0 until maskSeg).map { i =>
          val mask = if (port.masked) s"if (${port.prefix}mask[$i]) " else ""
          val ram_range = s"${(i + 1) * maskGran - 1}:${i * maskGran}"
          s"    ${mask}ram[${port.prefix}addr][$ram_range] <= ${port.prefix}$inputData[$ram_range];"
        } ++ Seq("  end")

        port.`type` match {
          case ReadPort  => genReadSequential(port.prefix + "en")
          case WritePort => genWriteSequential(port.prefix + "en", "data")
          case ReadWritePort =>
            genReadSequential(s"${port.prefix}en && !${port.prefix}wmode") ++
              genWriteSequential(s"${port.prefix}en && ${port.prefix}wmode", "wdata")
        }
      }
      val sequential = ports.flatMap(genSequential)

      def genCombinational(port: Port): Seq[String] = {
        val data = port.prefix + (if (port.`type` == ReadWritePort) "rdata" else "data")
        Seq(
          "`ifdef RANDOMIZE_GARBAGE_ASSIGN",
          s"reg [${((width - 1) / 32 + 1) * 32 - 1}:0] ${port.prefix}random;",
          "`ifdef RANDOMIZE_MEM_INIT",
          "  initial begin",
          "    #`RANDOMIZE_DELAY begin end",
          s"    ${port.prefix}random = {${Seq.fill((width - 1) / 32 + 1)("$random").mkString(", ")}};",
          s"    reg_${port.prefix}ren = ${port.prefix}random[0];",
          "  end",
          "`endif",
          s"always @(posedge ${port.prefix}clk) ${port.prefix}random <= {${Seq.fill((width - 1) / 32 + 1)("$random").mkString(", ")}};",
          s"assign $data = reg_${port.prefix}ren ? ram[reg_${port.prefix}addr] : ${port.prefix}random[${width - 1}:0];",
          "`else",
          s"assign $data = ram[reg_${port.prefix}addr];",
          "`endif"
        )
      }
      val combinational = readPorts.flatMap(genCombinational)

      s"""
         |module $name(
         |  ${portSpec.mkString(",\n  ")}
         |);
         |
         |  ${decl.mkString("\n  ")}
         |  ${sequential.mkString("\n  ")}
         |  ${combinational.mkString("\n  ")}
         |
         |endmodule""".stripMargin
    }

    def main(annotatedMemories: Seq[DefAnnotatedMemory]): Unit = {
      for (mem <- annotatedMemories) {
        val verilog = (genMem _).tupled(parse(mem))
        println(verilog)
      }
    }

    val annos = state.annotations.collect { case a: GenVerilogMemBehaviorModelAnno => a }
    annos match {
      case Nil => state
      case Seq(GenVerilogMemBehaviorModelAnno(mems)) =>
        main(mems)
        state
      case _ => error("Unexpected transform annotation")
    }
  }
}
