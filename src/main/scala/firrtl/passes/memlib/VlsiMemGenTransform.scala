package firrtl.passes.memlib

import firrtl.{CircuitState, DependencyAPIMigration, Transform}
import firrtl.stage.Forms

class VlsiMemGenTransform(outputConfig: String) extends Transform with DependencyAPIMigration {
  override def prerequisites = Forms.MidForm
  override protected def execute(state: CircuitState): CircuitState = {

    def parseLine(line: String): (String, Int, Int, Int, Int, Array[String]) = {
      val tokens = line.split(' ')
      val _ :: name :: _ :: depth :: _ :: width :: _ :: ports :: tail = tokens.toList
      val maskGran = (if (tail.isEmpty) width else tail.last).toInt
      (name, width.toInt, depth.toInt, maskGran, width.toInt / maskGran, ports.split(','))
    }

    // No `MaskedWritePort`, just `WritePort` with `masked` == true
    case class Port(prefix: String, `type`: MemPort, masked: Boolean)

    def parsePort(ports: Array[String]): Seq[Port] = {
      val readPorts = ports.filter(_ == "read")
      val writePorts = ports.zipWithIndex.filter { case (str, idx) => str == "write" || str == "mwrite" }.map(_._2)
      val rwPorts = ports.zipWithIndex.filter { case (str, idx) => str == "rw" || str == "mrw" }.map(_._2)

      readPorts.indices.map(number => Port(s"R${number}_", ReadPort, masked = true)) ++
        writePorts.zipWithIndex.map { case (idx, number) =>
          Port(s"W${number}_", WritePort, ports(idx).startsWith("m"))
        } ++
        rwPorts.zipWithIndex.map { case (idx, number) =>
          Port(s"RW${number}_", ReadWritePort, ports(idx).startsWith("m"))
        }
    }

    def genMem(name: String, width: Int, depth: Int, maskGran: Int, maskSeg: Int, _ports: Array[String]): String = {
      val addrWidth = math.max(math.ceil(math.log(depth) / math.log(2)).toInt, 1)
      val ports = parsePort(_ports)
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

    def main(confFile: String): Unit = {
      val file = io.Source.fromFile(confFile)
      for (line <- file.getLines()) {
        val parsedLine = (genMem _).tupled(parseLine(line))
        println(parsedLine)
      }
      file.close()
    }

    main(outputConfig)

    //todo
    state
  }
}
